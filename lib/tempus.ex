defmodule Tempus do
  @moduledoc """
  `Tempus` is a library to deal with timeslots.

  It aims to be a fast yet easy to use implementation of a schedule of any type,
    including but not limited to free/busy time schedules.

  The example of it might be a calendar software, where slots might be marked as
    free, or busy. It also allows simple arithmetics with schedules, like adding
    five days or subtracting 7 hours 30 minutes from now, considering busy slots.
  """

  use Tempus.Telemetria

  require Tempus.Slot
  alias Tempus.{Sigils.NilParser, Slot, Slots}

  import Tempus.Guards

  @typedoc "Direction for slots navigation"
  @type direction :: :fwd | :bwd
  @typedoc "Number of slots (`:stream` means lazy folding, unknown upfront)"
  @type count :: :infinity | :stream | non_neg_integer()
  @typedoc "Navigation option"
  @type option ::
          {:origin, Slot.origin()} | {:count, count() | neg_integer()} | {:direction, direction()}
  @typedoc "Argument containing navigation options"
  @type options :: [option()]
  @typep options_tuple :: {Slot.origin(), count(), 1 | -1}

  defdelegate slot(from, to), to: Tempus.Slot, as: :new
  defdelegate slot!(from, to), to: Tempus.Slot, as: :new!

  @doc """
  Syntactic sugar for `|> Enum.into(%Slots{})`.

  ## Examples

      iex> [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   Tempus.Slot.wrap(~D|2020-08-12|)
      ...> ] |> Tempus.slots()
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
          %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]},
          %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]},
          %Tempus.Slot{from: ~U[2020-08-12 00:00:00.000000Z], to: ~U[2020-08-12 23:59:59.999999Z]}]}}
  """
  defdelegate slots(enum), to: Slots, as: :wrap

  @doc """
  Helper to instantiate slot from any known format, by wrapping the argument.

  ## Examples

      iex> Tempus.guess("2023-04-10")
      {:ok, Tempus.Slot.wrap(~D[2023-04-10])}
      iex> Tempus.guess(nil)
      {:ok, %Tempus.Slot{from: nil, to: nil}}
      iex> Tempus.guess("∞")
      {:ok, %Tempus.Slot{from: nil, to: nil}}
      iex> Tempus.guess("")
      {:ok, %Tempus.Slot{from: nil, to: nil}}
      iex> Tempus.guess("2023-04-10T10:00:00Z")
      {:ok, Tempus.Slot.wrap(~U[2023-04-10T10:00:00Z])}
      iex> Tempus.guess("10:00:00")
      {:ok, Date.utc_today() |> DateTime.new!(Time.from_erl!({10, 0, 0})) |> Tempus.Slot.wrap()}
      iex> Tempus.guess("20230410T235007.123+0230")
      if Version.compare(System.version(), "1.14.0") == :lt do
        {:error, :invalid_format}
      else
        {:ok, Tempus.Slot.wrap(~U[2023-04-10T21:20:07.123Z])}
      end
      iex> Tempus.guess("2023-04-10-15")
      {:error, :invalid_format}
  """
  @spec guess(input :: nil | binary()) :: {:ok, Slot.t()} | {:error, any()}
  def guess(input) do
    input
    |> do_guess()
    |> case do
      {:ok, nil} -> {:ok, Slot.id()}
      {:ok, origin} -> {:ok, Slot.wrap(origin)}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Helper to instantiate slot from any known format, by joining the arguments.

  ## Examples

      iex> import Tempus.Sigils
      iex> Tempus.guess("2023-04-10", "2023-04-12")
      {:ok, ~I[2023-04-10 00:00:00.000000Z|2023-04-12 23:59:59.999999Z]}
      iex> Tempus.guess("2023-04-10", nil)
      {:ok, ~I[2023-04-10 00:00:00.000000Z|2023-04-10T23:59:59.999999Z]}
      iex> Tempus.guess("2023-04-10T10:00:00Z", "2023-04-12")
      {:ok, ~I[2023-04-10 10:00:00Z|2023-04-12 23:59:59.999999Z]}
      iex> Tempus.guess("20230410T235007.123+0230", "2023-04-12")
      if Version.compare(System.version(), "1.14.0") == :lt do
        {:error, {:invalid_arguments, [from: :invalid_format]}}
      else
        {:ok, ~I[2023-04-10 21:20:07.123Z|2023-04-12 23:59:59.999999Z]}
      end
      iex> Tempus.guess("2023-04-10", :ok)
      {:error, {:invalid_arguments, [to: :invalid_argument]}}
      iex> Tempus.guess(:ok, "2023-04-10")
      {:error, {:invalid_arguments, [from: :invalid_argument]}}
      iex> Tempus.guess("2023-04-10-15", :ok)
      {:error, {:invalid_arguments, [from: :invalid_format, to: :invalid_argument]}}
      iex> Tempus.guess("2023-20-40", "10:70:80")
      {:error, {:invalid_arguments, [from: :invalid_date, to: :invalid_time]}}
  """
  @spec guess(from :: nil | binary(), to :: nil | binary()) :: {:ok, Slot.t()} | {:error, any()}
  def guess(from, to) do
    [from, to]
    |> Enum.map(&guess/1)
    |> case do
      [{:ok, from}, {:ok, to}] -> {:ok, Slot.join(from, to)}
      [{:error, from}, {:error, to}] -> {:error, {:invalid_arguments, from: from, to: to}}
      [{:error, from}, _] -> {:error, {:invalid_arguments, from: from}}
      [_, {:error, to}] -> {:error, {:invalid_arguments, to: to}}
    end
  end

  defp do_guess("∞"), do: {:ok, nil}
  defp do_guess(""), do: {:ok, nil}

  defp do_guess(nil), do: {:ok, nil}

  defp do_guess(<<_::binary-size(4), ?-, _::binary-size(2), ?-, _::binary-size(2)>> = date),
    do: Date.from_iso8601(date)

  defp do_guess(<<_::binary-size(2), ?:, _::binary-size(2), ?:, _::binary-size(2)>> = time),
    do: Time.from_iso8601(time)

  if Version.compare(System.version(), "1.14.0") == :lt do
    defp do_guess(input) when is_binary(input) do
      [
        &with({:ok, value, _} <- DateTime.from_iso8601(&1), do: {:ok, value}),
        &Date.from_iso8601/1,
        &Time.from_iso8601/1,
        &NilParser.from_iso8601/1
      ]
      |> do_guess_reduce(input)
    end
  else
    defp do_guess(input) when is_binary(input) do
      [
        &with({:ok, value, _} <- DateTime.from_iso8601(&1, :extended), do: {:ok, value}),
        &with({:ok, value, _} <- DateTime.from_iso8601(&1, :basic), do: {:ok, value}),
        &Date.from_iso8601/1,
        &Time.from_iso8601/1,
        &NilParser.from_iso8601/1
      ]
      |> do_guess_reduce(input)
    end
  end

  defp do_guess(_), do: {:error, :invalid_argument}

  defp do_guess_reduce(attempts, input) do
    Enum.reduce_while(attempts, {:error, :invalid_format}, fn guesser, acc ->
      case guesser.(input) do
        {:ok, result} -> {:halt, {:ok, result}}
        {:error, :invalid_date} -> {:halt, {:error, :invalid_date}}
        {:error, :invalid_time} -> {:halt, {:error, :invalid_time}}
        _ -> {:cont, acc}
      end
    end)
  end

  @spec free?(slots :: Slots.t(), slot :: Slot.origin()) :: boolean()
  @doc """
  Checks whether the slot is disjoined against slots.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.free?(slots, ~D|2020-08-07|)
      false
      iex> Tempus.free?(slots, ~D|2020-08-08|)
      true
      iex> Tempus.free?(slots, ~U|2020-08-09T23:59:59.999999Z|)
      true
      iex> Tempus.free?(slots, DateTime.add(~U|2020-08-09T23:59:59.999999Z|, 1, :microsecond))
      false
  """
  def free?(%Slots{} = slots, origin) when is_origin(origin) do
    origin = Slot.wrap(origin)

    slots
    |> Slots.drop_until(origin)
    |> Enum.take(1)
    |> case do
      [] -> true
      [slot] when is_joint(slot, origin) -> false
      _ -> true
    end
  end

  @typedoc """
  The type defining how slicing is to be applied.
  When `:greedy`, overlapping boundary slots would be included,
    `:reluctant` would take only those fully contained in the interval.
  """
  @type slice_type :: :greedy | :reluctant
  @spec slice(
          slots :: Slots.t(),
          from :: Slots.locator(),
          to :: Slots.locator(),
          type :: slice_type()
        ) :: Slots.t()
  @doc since: "0.7.0"
  @doc """
  Slices the `%Slots{}` based on origins `from` and `to` and an optional type
    (default: `:reluctant`.) Returns sliced `%Slots{}` back.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> slots |> Tempus.slice(~D|2020-08-07|, ~D|2020-08-11|) |> Enum.count()
      1
  """
  def slice(slots, from, to, type \\ :reluctant)

  def slice(slots, nil, nil, _), do: slots

  def slice(slots, from, nil, type),
    do: Slots.drop_until(slots, from, greedy: type == :greedy)

  def slice(slots, nil, to, type),
    do: Slots.take_until(slots, to, greedy: type == :greedy)

  def slice(slots, from, to, type) do
    slots
    |> slice(from, nil, type)
    |> slice(nil, to, type)
  end

  @spec drop_while(slots :: Slots.t(), fun :: (Slot.t() -> boolean())) :: Slots.t()
  @doc since: "0.7.0"
  @doc """
  Drops slots at the beginning of the `%Slots{}` struct while `fun` returns a truthy value.

  ### Examples

      iex> import Tempus.Guards
      ...> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> slots
      ...> |> Tempus.drop_while(&is_coming_before(&1, Tempus.Slot.wrap(~D|2020-08-09|)))
      ...> |> Enum.count()
      1
  """
  def drop_while(%Slots{} = slots, fun) do
    Slots.drop_until(slots, &(not fun.(&1)))
  end

  @spec take_while(slots :: Slots.t(), fun :: (Slot.t() -> boolean())) :: Slots.t()
  @doc since: "0.7.0"
  @doc """
  Takes slots at the beginning of the `%Slots{}` struct while `fun` returns a truthy value.

  ### Examples

      iex> import Tempus.Guards
      ...> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> slots
      ...> |> Tempus.take_while(&is_coming_before(&1, Tempus.Slot.wrap(~D|2020-08-09|)))
      ...> |> Enum.count()
      1
  """
  def take_while(%Slots{} = slots, fun) do
    Slots.take_until(slots, &(not fun.(&1)))
  end

  @spec days_add(slots :: Slots.t(), opts :: options()) :: [Date.t()]
  @doc since: "0.2.0"
  @doc """
  Returns the reversed list of free days after origin.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.days_add(slots, origin: ~D|2020-08-07|, count: 3) |> hd()
      ~D|2020-08-12|
      iex> Tempus.days_add(slots, origin: ~D|2020-08-07|, count: 3, direction: :fwd) |> hd()
      ~D|2020-08-12|
      iex> Tempus.days_add(slots, origin: ~D|2020-08-07|, count: -3, direction: :bwd) |> hd()
      ~D|2020-08-12|
      iex> Tempus.days_add(slots, origin: ~D|2020-08-12|, count: -4) |> hd()
      ~D|2020-08-06|
      iex> Tempus.days_add(slots, origin: ~D|2020-08-12|, count: 4, direction: :bwd) |> hd()
      ~D|2020-08-06|
      iex> Tempus.days_add(slots, origin: ~D|2020-08-12|, count: -4, direction: :fwd) |> hd()
      ~D|2020-08-06|
  """
  def days_add(slots, opts \\ []) do
    {origin, count, iterator} = options(opts)

    (iterator == -1)
    |> if(do: origin.from, else: origin.to)
    |> DateTime.to_date()
    |> Stream.iterate(&Date.add(&1, iterator))
    |> Enum.reduce_while([], fn
      _date, acc when length(acc) > count -> {:halt, acc}
      date, acc -> {:cont, if(free?(slots, date), do: [date | acc], else: acc)}
    end)
  end

  @doc deprecated: "Use days_add/2 instead"
  @spec days_ahead(slots :: Slots.t(), origin :: Date.t(), count :: integer()) :: [Date.t()]
  @doc """
  Returns the reversed list of free days after origin.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.days_ahead(slots, ~D|2020-08-07|, 0)
      [~D|2020-08-08|]
      iex> Tempus.days_ahead(slots, ~D|2020-08-07|, 3) |> hd()
      ~D|2020-08-12|
  """
  def days_ahead(slots, origin, count) when is_integer(count) and count >= 0,
    do: days_add(slots, origin: origin, count: count, direction: :fwd)

  @doc deprecated: "Use days_add/2 with negative count or `:bwd` forth parameter instead"
  @spec days_ago(slots :: Slots.t(), origin :: Date.t(), count :: integer()) :: [Date.t()]
  @doc """
  Returns the reversed list of free days after origin.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.days_ago(slots, ~D|2020-08-07|, 0)
      [~D|2020-08-06|]
      iex> Tempus.days_ago(slots, ~D|2020-08-12|, 4) |> hd()
      ~D|2020-08-06|
  """
  def days_ago(slots, origin, count) when is_integer(count) and count >= 0,
    do: days_add(slots, origin: origin, count: count, direction: :bwd)

  @spec next_busy(Slots.t(), options()) :: [Slot.t()] | Slot.t() | nil | no_return
  @doc deprecated: "Use `slice/3` instead"
  @doc """
  Returns the next **busy** slot from the slots passed as a first argument,
    that immediately follows `origin`. If slots are overlapped, the overlapped
    one gets returned.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-08 23:00:00Z|, to: ~U|2020-08-09 12:00:00Z|})
      %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-07 12:00:00Z|}, count: 2) |> hd()
      %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-08 12:00:00Z|})
      %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-10 12:00:00Z|})
      %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-07|)
      %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-08|)
      %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-08|, direction: :bwd)
      %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-10|, direction: :bwd)
      %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-10|, direction: :bwd, count: :infinity)
      slots
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-10|, count: 3.1415)
      ** (Tempus.ArgumentError) invalid argument: expected ‹Elixir.Integer›, got: ‹3.1415›
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-10|, direction: :bwd, count: 2)
      Enum.to_list(slots)
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-10|, direction: :bwd, count: 1)
      Enum.drop(slots, 1)
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-11 11:00:00Z|, to: ~U|2020-08-11 12:00:00Z|})
      nil
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-06 11:00:00Z|, to: ~U|2020-08-06 12:00:00Z|}, direction: :bwd)
      nil
      iex> Tempus.next_busy(%Tempus.Slots{})
      nil
  """
  @telemetria level: :debug
  def next_busy(slots, opts \\ [])

  def next_busy(%Slots{} = slots, opts) do
    {origin, count, iterator} = options(opts)
    do_next_busy(slots, origin, count, iterator)
  end

  defp do_next_busy(slots, origin, :infinity, 1) do
    Slots.drop_until(slots, origin, greedy: true)
  end

  defp do_next_busy(slots, origin, :infinity, -1) do
    Slots.take_until(slots, origin, greedy: true)
  end

  defp do_next_busy(slots, origin, 0, 1) do
    slots
    |> Slots.drop_until(origin, greedy: true)
    |> Enum.take(1)
    |> List.last()
  end

  defp do_next_busy(slots, origin, 0, -1) do
    slots
    |> Slots.drop_until(origin, adjustment: -1, greedy: true)
    |> Enum.take(2)
    |> then(fn
      [_, joint] when is_joint(joint, origin) -> joint
      [slot | _] when is_coming_before(slot, origin) -> slot
      _ -> nil
    end)
  end

  defp do_next_busy(slots, origin, count, 1) do
    slots
    |> Slots.drop_until(origin, greedy: true)
    |> Enum.take(count)
  end

  defp do_next_busy(slots, origin, count, -1) do
    slots
    |> Slots.drop_until(origin, adjustment: -count + 1, greedy: true)
    # |> Slots.take_while(count)
    |> Enum.take(count)
  end

  @spec next_free(Slots.t(), options()) :: [Slot.t()] | Slot.t() | no_return
  @doc deprecated: "Use `slice/3` instead"
  @doc """
  Returns the next **free** slot from the slots passed as a first argument,
    that immediately follows `origin`. If slots are overlapped, the overlapped
    one gets returned.

  ### Examples

      iex> import Tempus.Sigils
      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   Tempus.Slot.wrap(~D|2020-08-12|),
      ...>   Tempus.Slot.wrap(~D|2020-08-14|),
      ...>   Tempus.Slot.wrap(~D|2030-08-14|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.next_free(slots, origin: %Tempus.Slot{from: ~U|2020-08-08 23:00:00Z|, to: ~U|2020-08-09 12:00:00Z|})
      ~I[2020-08-08 00:00:00.000000Z → 2020-08-09 23:59:59.999999Z]
      iex> Tempus.next_free(slots, origin: %Tempus.Slot{from: ~U|2020-08-06 11:00:00Z|, to: ~U|2020-08-06 12:00:00Z|})
      ~I[∞ → 2020-08-06 23:59:59.999999Z]nu
      iex> Tempus.next_free(slots, origin: ~U|2020-08-13 01:00:00.000000Z|)
      ~I[2020-08-13 00:00:00.000000Z → 2020-08-13 23:59:59.999999Z]
      iex> Tempus.next_free(slots, origin: ~D|2020-08-13|)
      ~I[2020-08-13 00:00:00.000000Z → 2020-08-13 23:59:59.999999Z]
      iex> Tempus.next_free(slots, origin: ~D|2020-08-14|)
      ~I[2020-08-15 00:00:00.000000Z → 2030-08-13 23:59:59.999999Z]
      iex> Tempus.next_free(slots)
      ~I[2020-08-15 00:00:00.000000Z → 2030-08-13 23:59:59.999999Z]
  """
  @telemetria level: :debug
  def next_free(slots, opts \\ [])

  def next_free(%Slots{} = slots, opts) do
    slots
    |> Slots.inverse()
    |> next_busy(opts)
  end

  @doc """
  Adds an amount of units to the origin, considering slots given.

  ### Examples

      iex> slots = [
      ...>   ~D|2020-08-07|,
      ...>   ~D|2020-08-10|,
      ...>   ~D|2020-08-11|,
      ...>   ~D|2020-08-14|
      ...> ] |> Enum.into(Tempus.Slots.new(:stream, []))
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, 0, :second)
      ~U[2020-08-12 00:00:00Z]
      iex> Tempus.add(slots, ~U|2020-08-12 01:00:00Z|, 0, :second)
      ~U[2020-08-12 01:00:00Z]
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, -10*60+1, :second)
      ~U[2020-08-09 23:50:00Z]
      iex> Tempus.add(slots, ~U|2020-08-12 00:09:00Z|, -10*60, :second)
      ~U[2020-08-09 23:59:00Z]
      iex> Tempus.add(slots, ~U|2020-08-12 00:10:00Z|, -10*60, :second)
      ~U[2020-08-12 00:00:00Z]
      iex> Tempus.add(slots, ~U|2020-08-12 00:10:00Z|, -10*60-1, :second)
      ~U[2020-08-09 23:59:59Z]
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, 10*60, :second)
      ~U[2020-08-12 00:10:00Z]
      iex> Tempus.add(slots, ~U|2020-08-12 00:00:00Z|, 10*60, :second)
      ~U[2020-08-12 00:10:00Z]
      iex> Tempus.add(slots, ~U|2020-08-09 23:55:00Z|, 10*60, :second)
      ~U[2020-08-12 00:05:00Z]
      iex> Tempus.add(slots, ~U|2020-08-08 23:55:00Z|, 10*60, :second)
      ~U[2020-08-09 00:05:00Z]
      iex> Tempus.add(slots, ~U|2020-08-06 23:55:00Z|, 2*3600*24 + 10*60, :second)
      ~U[2020-08-12 00:05:00Z]

      iex> slots = Tempus.Slots.new(:stream, [])
      iex> Tempus.add(slots, ~U|2020-08-12 01:00:00Z|, 0, :second)
      ~U[2020-08-12 01:00:00Z]
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, 5*60+1, :second)
      ~U[2020-08-11 23:05:01.000000Z]
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, -10*60+1, :second)
      ~U[2020-08-11 22:50:01Z]

      iex> slots = Tempus.Slots.new(:list, [])
      iex> Tempus.add(slots, ~U|2020-08-12 01:00:00Z|, 0, :second)
      ~U[2020-08-12 01:00:00Z]
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, 5*60+1, :second)
      ~U[2020-08-11 23:05:01Z]
      iex> Tempus.add(slots, ~U|2020-08-11 23:00:00Z|, -10*60+1, :second)
      ~U[2020-08-11 22:50:01Z]
      iex> slots |> Tempus.add(1) |> DateTime.to_date()
      Date.utc_today()
  """
  @spec add(
          slots :: Slots.t(),
          origin :: DateTime.t(),
          amount_to_add :: integer(),
          unit :: System.time_unit()
        ) :: DateTime.t()
  @telemetria level: :debug
  def add(slots, origin \\ DateTime.utc_now(), amount_to_add, unit \\ :second)

  def add(%Slots{slots: %Slots.List{slots: []}}, origin, amount_to_add, unit) do
    DateTime.add(origin, amount_to_add, unit)
  end

  def add(slots, origin, 0, unit) do
    case next_free(slots, origin: origin) do
      %{from: %DateTime{} = from} ->
        [from, origin]
        |> Enum.max(DateTime)
        |> DateTime.truncate(unit)

      _ ->
        origin
    end
  end

  def add(slots, origin, amount_to_add, unit) when amount_to_add > 0 do
    amount_in_microseconds = System.convert_time_unit(amount_to_add, unit, :microsecond)

    slots
    |> next_free(origin: origin, count: :infinity, direction: :fwd)
    |> Enum.reduce_while({origin, amount_in_microseconds}, fn
      %Slot{from: from, to: to}, {dt, ms} ->
        from = [from, dt] |> Enum.reject(&is_nil/1) |> Enum.max(DateTime)

        if is_nil(to) or
             Slot.duration(%Slot{from: from, to: to}, :microsecond) > ms do
          {:halt, DateTime.add(from, ms, :microsecond)}
        else
          {:cont, {to, ms - Slot.duration(%Slot{from: from, to: to}, :microsecond)}}
        end
    end)
    |> case do
      %DateTime{} = result ->
        DateTime.truncate(result, unit)

      {dt, rest} when is_integer(rest) ->
        DateTime.add(dt, rest, :microsecond)
    end
  end

  def add(slots, origin, amount_to_add, unit) when amount_to_add < 0 do
    amount_in_microseconds = System.convert_time_unit(-amount_to_add, unit, :microsecond)

    slots
    |> Slots.inverse()
    |> Enum.reduce_while([], fn
      %Slot{from: nil} = slot, slots ->
        {:cont, [slot | slots]}

      %Slot{from: from} = _slot, collected when is_coming_before(origin, from) ->
        {:halt, do_calc_subtract(collected, amount_in_microseconds)}

      %Slot{to: to} = slot, collected when is_coming_before(to, origin) ->
        collected =
          [slot | collected]
          |> Enum.reduce_while({[], 0}, fn
            _slot, {collected, ms} when ms >= amount_in_microseconds ->
              {:halt, {collected, ms}}

            slot, {collected, ms} ->
              {:cont, {[slot | collected], ms + Slot.duration(slot, :microsecond)}}
          end)
          |> elem(0)
          |> Enum.reverse()

        {:cont, collected}

      %Slot{from: from}, collected ->
        slot = %Slot{from: from, to: origin}
        {:halt, do_calc_subtract([slot | collected], amount_in_microseconds)}
    end)
    |> case do
      %DateTime{} = dt -> DateTime.truncate(dt, unit)
      [] -> DateTime.add(origin, amount_to_add, unit)
      nil -> nil
      [%Slot{from: nil, to: nil}] -> DateTime.add(origin, amount_to_add, unit)
    end
  end

  defp do_calc_subtract(slots, amount) do
    Enum.reduce_while(slots, amount, fn %Slot{} = slot, ms ->
      duration = Slot.duration(slot, :microsecond)

      if duration < ms,
        do: {:cont, ms - duration},
        else: {:halt, DateTime.add(slot.to, -ms, :microsecond)}
    end)
  end

  @spec options(opts :: options()) :: options_tuple()
  defp options(opts) when is_list(opts) do
    origin =
      opts
      |> Keyword.get(:origin, DateTime.utc_now())
      |> Slot.wrap()

    case Keyword.get(opts, :count, 0) do
      atom when is_atom(atom) ->
        {origin, atom, if(Keyword.get(opts, :direction, :fwd) == :bwd, do: -1, else: 1)}

      count when is_integer(count) ->
        iterator =
          if :erlang.xor(count < 0, Keyword.get(opts, :direction, :fwd) == :bwd), do: -1, else: 1

        {origin, abs(count), iterator}

      other ->
        raise(Tempus.ArgumentError, expected: Integer, passed: other)
    end
  end
end
