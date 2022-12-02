defmodule Tempus do
  @moduledoc """
  Documentation for `Tempus`.
  """

  use Boundary, exports: [Slot, Slots]
  use Tempus.Telemetria, action: :import

  alias Tempus.{Slot, Slots}

  @type direction :: :fwd | :bwd
  @type count :: :infinity | :stream | non_neg_integer()
  @type option ::
          {:origin, Slot.origin()} | {:count, count() | neg_integer()} | {:direction, direction()}
  @type options :: [option()]
  @typep options_tuple :: {Slot.origin(), count(), 1 | -1}

  @spec free?(slots :: Slots.t(), slot :: Slot.origin(), method :: :smart | :size) ::
          boolean() | no_return
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
  """
  def free?(slots, slot, method \\ :smart)

  def free?(%Slots{} = slots, %Slot{} = slot, :size),
    do: Slots.size(Slots.add(slots, slot)) == Slots.size(slots) + 1

  def free?(%Slots{slots: []}, %Slot{}, :smart), do: true

  def free?(%Slots{slots: slots}, %Slot{} = origin, :smart) do
    slots
    |> Enum.reduce_while(nil, fn
      %Slot{} = current, nil ->
        if Date.compare(current.from, origin.to) == :gt,
          do: {:halt, current},
          else: {:cont, current}

      %Slot{} = current, %Slot{} = previous ->
        if DateTime.compare(previous.to, origin.from) == :lt do
          if DateTime.compare(current.to, origin.from) == :lt do
            {:cont, current}
          else
            if DateTime.compare(current.from, origin.to) == :gt do
              {:halt, current}
            else
              {:halt, nil}
            end
          end
        else
          {:halt, nil}
        end
    end)
    |> case do
      nil -> false
      %Slot{} -> true
    end
  end

  def free?(%Slots{} = slots, slot, method), do: free?(slots, Slot.wrap(slot), method)

  @typedoc """
  The type defining how slicing is to be applied.
  When `:greedy`, overlapping boundary slots would be included,
    `:reluctant` would take only those fully contained in the interval.
  """
  @type slice_type :: :greedy | :reluctant
  @spec slice(
          slots :: Slots.t(),
          from :: Slot.origin(),
          to :: Slot.origin(),
          type :: slice_type()
        ) :: Slots.t()
  @doc since: "0.7.0"
  @doc """
  Slices the `%Slots{}` based on origins `from` and `to` and an optional type
    (default: `:reluctant`.) Returns sliced `%Slots{}` back.
  """
  def slice(slots, from, to, type \\ :reluctant)
  def slice(slots, nil, nil, _), do: slots

  def slice(slots, from, nil, :greedy) do
    slots
    |> Enum.drop_while(&(Slot.compare(&1, from) == :lt))
    |> Slots.wrap_unsafe()
  end

  def slice(slots, from, nil, :reluctant) do
    slots
    |> slice(from, nil, :greedy)
    |> Enum.drop_while(&(Slot.compare(&1, from) != :gt))
    |> Slots.wrap_unsafe()
  end

  def slice(slots, nil, to, :greedy) do
    slots
    |> Enum.take_while(&(Slot.compare(&1, to) != :gt))
    |> Slots.wrap_unsafe()
  end

  def slice(slots, nil, to, :reluctant) do
    slots
    |> Enum.take_while(&(Slot.compare(&1, to) == :lt))
    |> Slots.wrap_unsafe()
  end

  def slice(slots, from, to, type) do
    slots
    |> slice(from, nil, type)
    |> slice(nil, to, type)
  end

  @spec days_add(slots :: Slots.t(), opts :: options()) :: Date.t()
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
  @spec days_ahead(slots :: Slots.t(), origin :: Date.t(), count :: integer()) :: Date.t()
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
  @spec days_ago(slots :: Slots.t(), origin :: Date.t(), count :: integer()) :: Date.t()
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
    that immediately follows `origin`. IOf slots are overlapped, the overlapped
    one gets returned.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-08 23:00:00Z|, to: ~U|2020-08-09 12:00:00Z|})
      #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-07 12:00:00Z|}, count: 2) |> hd()
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-08 12:00:00Z|})
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-07 11:00:00Z|, to: ~U|2020-08-10 12:00:00Z|})
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-07|)
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-08|)
      #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-08|, direction: :bwd)
      #Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: ~D|2020-08-10|, direction: :bwd)
      #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-11 11:00:00Z|, to: ~U|2020-08-11 12:00:00Z|})
      nil
      iex> Tempus.next_busy(slots, origin: %Tempus.Slot{from: ~U|2020-08-06 11:00:00Z|, to: ~U|2020-08-06 12:00:00Z|}, direction: :bwd)
      nil
      iex> Tempus.next_busy(%Tempus.Slots{})
      nil
  """
  @telemetria level: :debug
  def next_busy(slots, opts \\ [])

  def next_busy(%Slots{} = slots, opts),
    do: do_next_busy(slots, options(opts))

  def next_busy(%Slot{} = slot, opts),
    do: next_busy(Slots.wrap(slot), opts)

  @spec do_next_busy(Slots.t(), options_tuple()) :: [Slot.t()]
  defp do_next_busy(%Slots{} = slots, {origin, count, iterator}) when count >= 0 do
    {slots, comparator} = if iterator == -1, do: {Enum.reverse(slots), :gt}, else: {slots, :lt}

    slots
    |> Enum.drop_while(fn
      %Slot{} = slot -> Slot.strict_compare(slot, origin) == comparator
      other -> raise Tempus.ArgumentError, expected: Tempus.Slot, passed: other
    end)
    |> wrap_result(count)
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

  def next_free(%Slots{slots: slots}, opts),
    do: do_next_free(slots, options(opts))

  defp do_next_free([], {origin, 0, -1}),
    do: %Slot{from: nil, to: Slot.shift(origin, from: -1, unit: :microsecond).from}

  defp do_next_free([], {origin, _count, -1}),
    do: [%Slot{from: nil, to: Slot.shift(origin, from: -1, unit: :microsecond).from}]

  defp do_next_free([], {origin, 0, 1}),
    do: %Slot{from: Slot.shift(origin, to: 1, unit: :microsecond).to, to: nil}

  defp do_next_free([], {origin, _count, 1}),
    do: [%Slot{from: Slot.shift(origin, to: 1, unit: :microsecond).to, to: nil}]

  defp do_next_free(slots, {origin, count, 1}) do
    slots
    |> do_add_infinite_slots(origin)
    |> Enum.chunk_every(2, 1)
    |> Enum.reduce_while([], fn
      _, acc when (count == 0 and length(acc) > 0) or (length(acc) >= count and count != 0) ->
        {:halt, acc}

      [%Slot{to: from}, %Slot{from: to}], acc ->
        free_slot = Slot.shift(%Slot{from: from, to: to}, from: 1, to: -1)

        if Slot.cover?(free_slot, origin) or Slot.strict_compare(free_slot, origin) == :gt,
          do: {:cont, [free_slot | acc]},
          else: {:cont, acc}

      [%Slot{to: from}], acc ->
        free_slot = Slot.shift(%Slot{from: from}, from: 1)

        if Slot.cover?(free_slot, origin) or Slot.strict_compare(free_slot, origin) == :gt,
          do: {:cont, [free_slot | acc]},
          else: {:cont, acc}
    end)
    |> Enum.sort({:asc, Slot})
    |> wrap_result(count)
  end

  defp do_next_free(slots, {origin, count, -1}) do
    slots
    |> do_add_infinite_slots(origin)
    |> Enum.reverse()
    |> Enum.chunk_every(2, 1)
    |> Enum.reduce_while([], fn
      _, acc when (count == 0 and length(acc) > 0) or (length(acc) >= count and count != 0) ->
        {:halt, acc}

      [%Slot{from: to}, %Slot{to: from}], acc ->
        free_slot = Slot.shift(%Slot{from: from, to: to}, from: 1, to: -1)

        if Slot.cover?(free_slot, origin) or Slot.strict_compare(free_slot, origin) == :lt,
          do: {:cont, [free_slot | acc]},
          else: {:cont, acc}

      [%Slot{from: to}], acc ->
        free_slot = Slot.shift(%Slot{to: to}, to: -1)

        if Slot.cover?(free_slot, origin) or Slot.strict_compare(free_slot, origin) == :lt,
          do: {:cont, [free_slot | acc]},
          else: {:cont, acc}
    end)
    |> Enum.sort({:desc, Slot})
    |> wrap_result(count)
  end

  @spec do_infinite_slot_after(origin :: Slot.t()) :: Slot.t()
  defp do_infinite_slot_after(origin),
    do: %Slot{
      from: Slot.shift(origin, to: 1, unit: :microsecond).to,
      to: nil
    }

  @spec do_infinite_slot_before(origin :: Slot.t()) :: Slot.t()
  defp do_infinite_slot_before(origin),
    do: %Slot{
      from: nil,
      to: Slot.shift(origin, from: -1, unit: :microsecond).from
    }

  @spec do_add_infinite_slots(slots :: [Slot.t()], origin :: Slot.t()) :: [Slot.t()]
  defp do_add_infinite_slots([], origin),
    do: [do_infinite_slot_before(origin), do_infinite_slot_after(origin)]

  defp do_add_infinite_slots([first | _] = slots, origin) do
    last = List.last(slots)

    slots =
      if Slot.compare(origin, first) == :lt,
        do: [do_infinite_slot_before(origin) | slots],
        else: slots

    if Slot.compare(origin, last) == :gt,
      do: slots ++ [do_infinite_slot_after(origin)],
      else: slots
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
    |> Enum.reduce_while(amount_in_microseconds, fn %Slot{} = slot, rest_to_add_in_microseconds ->
      maybe_result = DateTime.add(slot.from, rest_to_add_in_microseconds, :microsecond)

      if is_nil(slot.to) or DateTime.compare(maybe_result, slot.to) != :gt,
        do: {:halt, maybe_result},
        else: {:cont, rest_to_add_in_microseconds - Slot.duration(slot, :microsecond)}
    end)
    |> DateTime.truncate(unit)
  end

  def add(slots, origin, amount_to_add, unit) when amount_to_add < 0 do
    amount_in_microseconds = System.convert_time_unit(amount_to_add, unit, :microsecond)

    [slot | slots] = next_free(slots, origin: origin, count: :infinity, direction: :bwd)

    [%Slot{slot | to: origin} | slots]
    |> Enum.reduce_while(amount_in_microseconds, fn %Slot{} = slot, rest_to_add_in_microseconds ->
      maybe_result = DateTime.add(slot.to, rest_to_add_in_microseconds, :microsecond)

      if is_nil(slot.from) or DateTime.compare(maybe_result, slot.from) != :lt,
        do: {:halt, maybe_result},
        else: {:cont, rest_to_add_in_microseconds + Slot.duration(slot, :microsecond)}
    end)
    |> DateTime.truncate(unit)
  end

  @spec wrap_result(slots :: Enumerable.t(), count :: count()) :: Slot.t() | Enumerable.t()
  # defp wrap_result(%Slots{slots: slots}, count), do: wrap_result(slots, count)
  # defp wrap_result(%Stream{} = slots, :stream), do: slots
  # defp wrap_result(slots, :stream) when is_function(slots), do: slots
  defp wrap_result(slots, :stream), do: Stream.map(slots, & &1)
  defp wrap_result(slots, :infinity), do: slots
  defp wrap_result(slots, 0), do: Enum.at(slots, 0)
  defp wrap_result(slots, count) when is_integer(count) and count > 0, do: Enum.take(slots, count)

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
