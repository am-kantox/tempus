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

  alias Tempus.{Slot, Slots}

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
      iex> Tempus.guess("2023-04-10T10:00:00Z")
      {:ok, Tempus.Slot.wrap(~U[2023-04-10T10:00:00Z])}
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
      iex> Tempus.guess("2023-04-10T10:00:00Z", "2023-04-12")
      {:ok, ~I[2023-04-10 10:00:00Z|2023-04-12 23:59:59.999999Z]}
      iex> Tempus.guess("20230410T235007.123+0230", "2023-04-12")
      if Version.compare(System.version(), "1.14.0") == :lt do
        {:error, {:invalid_arguments, [from: :invalid_format]}}
      else
        {:ok, ~I[2023-04-10 21:20:07.123Z|2023-04-12 23:59:59.999999Z]}
      end
      iex> Tempus.guess("2023-04-10-15", :ok)
      {:error, {:invalid_arguments, [from: :invalid_format, to: :invalid_argument]}}
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
        &Time.from_iso8601/1
      ]
      |> do_guess_reduce(input)
    end
  else
    defp do_guess(input) when is_binary(input) do
      [
        &with({:ok, value, _} <- DateTime.from_iso8601(&1, :extended), do: {:ok, value}),
        &with({:ok, value, _} <- DateTime.from_iso8601(&1, :basic), do: {:ok, value}),
        &Date.from_iso8601/1,
        &Time.from_iso8601/1
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

  @spec drop_while(slots :: Slots.t(), fun :: (Slot.t() -> as_boolean(term))) :: Slots.t()
  @doc since: "0.7.0"
  @doc """
  Drops slots at the beginning of the `%Slots{}` struct while `fun` returns a truthy value.
  """
  def drop_while(%Slots{slots: slots}, fun) do
    Slots.drop_until(slots, &(not fun.(&1)))
  end

  @spec take_while(slots :: Slots.t(), fun :: (Slot.t() -> as_boolean(term))) :: Slots.t()
  @doc since: "0.7.0"
  @doc """
  Takes slots at the beginning of the `%Slots{}` struct while `fun` returns a truthy value.
  """
  def take_while(%Slots{slots: slots}, fun) do
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

    slots
    |> Slots.drop_until(origin, adjustment: count * iterator, greedy: true)
    |> Enum.take(count + 1)
    |> Enum.drop(count)
    |> List.first()
  end

  @spec next_free(Slots.t(), options()) :: [Slot.t()] | Slot.t() | no_return
  @doc """
  Returns the next **free** slot from the slots passed as a first argument,
    that immediately follows `origin`. If slots are overlapped, the overlapped
    one gets returned.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   Tempus.Slot.wrap(~D|2020-08-12|),
      ...>   Tempus.Slot.wrap(~D|2020-08-14|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.next_free(slots, origin: %Tempus.Slot{from: ~U|2020-08-08 23:00:00Z|, to: ~U|2020-08-09 12:00:00Z|})
      #Slot<[from: ~U[2020-08-08 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]]>
      iex> Tempus.next_free(slots, origin: %Tempus.Slot{from: ~U|2020-08-06 11:00:00Z|, to: ~U|2020-08-06 12:00:00Z|})
      #Slot<[from: ~U[2020-08-06 11:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]]>
      iex> Tempus.next_free(slots, origin: ~U|2020-08-13 01:00:00.000000Z|)
      #Slot<[from: ~U[2020-08-13 00:00:00.000000Z], to: ~U[2020-08-13 23:59:59.999999Z]]>
      iex> Tempus.next_free(slots, origin: ~D|2020-08-13|)
      #Slot<[from: ~U[2020-08-13 00:00:00.000000Z], to: ~U[2020-08-13 23:59:59.999999Z]]>
      iex> Tempus.next_free(slots, origin: ~D|2020-08-14|)
      #Slot<[from: ~U[2020-08-15 00:00:00.000000Z], to: nil]>
      iex> Tempus.next_free(slots, origin: ~D|2020-08-07|, count: 5)
      [
        %Tempus.Slot{from: ~U[2020-08-08 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-13 00:00:00.000000Z], to: ~U[2020-08-13 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-15 00:00:00.000000Z], to: nil}
      ]
      iex> Tempus.next_free(slots, origin: ~D|2020-08-15|, count: -5)
      [
        %Tempus.Slot{from: ~U[2020-08-15 00:00:00.000000Z], to: ~U[2020-08-15 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-13 00:00:00.000000Z], to: ~U[2020-08-13 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-08 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: nil, to: ~U[2020-08-06 23:59:59.999999Z]},
      ]
      iex> Tempus.next_free(slots, origin: ~D|2020-08-12|, count: :infinity, direction: :bwd)
      [
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-08 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: nil, to: ~U[2020-08-06 23:59:59.999999Z]}
      ]
  """
  @telemetria level: :debug
  def next_free(slots, opts \\ [])

  def next_free(%Slots{} = slots, opts) do
    {origin, count, iterator} = options(opts)

    tail =
      slots
      |> Slots.drop_until(origin, adjustment: count * iterator - 1, greedy: false)
      |> Enum.take(count + 2)

    Slots.List.inverse(%Slots.List{slots: tail}).slots
    |> Enum.slice(1, count + 1)
  end

  @doc """
  Adds an amount of units to the origin, considering slots given.
  """
  @spec add(
          slots :: Slots.t(),
          origin :: DateTime.t(),
          amount_to_add :: integer(),
          unit :: System.time_unit()
        ) :: DateTime.t()
  @telemetria level: :debug
  def add(slots, origin \\ DateTime.utc_now(), amount_to_add, unit \\ :second)

  def add(%Slots{slots: []}, origin, amount_to_add, unit) do
    DateTime.add(origin, amount_to_add, unit)
  end

  def add(slots, origin, 0, unit) do
    %{from: from} = next_free(slots, origin: origin)

    [from, origin]
    |> Enum.max(DateTime)
    |> DateTime.truncate(unit)
  end

  def add(slots, origin, amount_to_add, unit) when amount_to_add > 0 do
    amount_in_microseconds = System.convert_time_unit(amount_to_add, unit, :microsecond)

    [slot | slots] = next_free(slots, origin: origin, count: :infinity, direction: :fwd)

    [%Slot{slot | from: origin} | slots]
    |> Enum.reduce_while({origin, amount_in_microseconds}, fn
      %Slot{} = slot, {_, rest_to_add_in_microseconds} ->
        maybe_result = DateTime.add(slot.from, rest_to_add_in_microseconds, :microsecond)

        if is_nil(slot.to) or DateTime.compare(maybe_result, slot.to) != :gt,
          do: {:halt, maybe_result},
          else:
            {:cont,
             {maybe_result, rest_to_add_in_microseconds - Slot.duration(slot, :microsecond)}}
    end)
    |> case do
      %DateTime{} = result ->
        DateTime.truncate(result, unit)

      {%DateTime{} = result, rest} when is_integer(rest) ->
        DateTime.add(result, rest, :microsecond)
    end
  end

  def add(slots, origin, amount_to_add, unit) when amount_to_add < 0 do
    amount_in_microseconds = System.convert_time_unit(amount_to_add, unit, :microsecond)

    [slot | slots] = next_free(slots, origin: origin, count: :infinity, direction: :bwd)

    [%Slot{slot | to: origin} | slots]
    |> Enum.reduce_while({origin, amount_in_microseconds}, fn
      %Slot{} = slot, {_, rest_to_add_in_microseconds} ->
        maybe_result = DateTime.add(slot.to, rest_to_add_in_microseconds, :microsecond)

        if is_nil(slot.from) or DateTime.compare(maybe_result, slot.from) != :lt,
          do: {:halt, maybe_result},
          else:
            {:cont,
             {maybe_result, rest_to_add_in_microseconds + Slot.duration(slot, :microsecond)}}
    end)
    |> case do
      %DateTime{} = result ->
        DateTime.truncate(result, unit)

      {%DateTime{} = result, rest} when is_integer(rest) ->
        DateTime.add(result, rest, :microsecond)
    end
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
