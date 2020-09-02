defmodule Tempus do
  @moduledoc """
  Documentation for `Tempus`.
  """

  use Boundary, exports: [Slot, Slots]

  alias Tempus.{Slot, Slots}

  @type direction :: :fwd | :bwd
  @type option :: {:origin, Slot.origin()} | {:count, integer()} | {:direction, direction()}
  @type options :: [option()]
  @typep options_tuple :: {Slot.origin(), integer(), 1 | -1}

  @spec free?(slots :: Slots.t(), slot :: Slot.origin()) :: boolean() | no_return
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
  def free?(%Slots{} = slots, slot) do
    Slots.size(Slots.add(slots, Slot.wrap(slot))) == Slots.size(slots) + 1
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
  def next_busy(slots, opts \\ [])

  def next_busy(%Slots{} = slots, opts),
    do: do_next_busy(slots, options(opts))

  def next_busy(%Slot{} = slot, opts),
    do: next_busy(Slots.add(%Slots{}, slot), opts)

  @spec do_next_busy(Slots.t(), options_tuple()) :: [Slot.t()]
  defp do_next_busy(%Slots{} = slots, {origin, count, iterator}) when count >= 0 do
    {slots, comparator} =
      if iterator == -1, do: {Enum.reverse(slots), :gt}, else: {Enum.to_list(slots), :lt}

    result =
      Enum.drop_while(slots, fn
        %Slot{} = slot -> Slot.strict_compare(slot, origin) == comparator
        other -> raise Tempus.ArgumentError, expected: Tempus.Slot, passed: other
      end)

    if count == 0, do: List.first(result), else: Enum.take(result, count)
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
      #Slot<[from: ~U[2020-08-06 11:00:00Z], to: ~U[2020-08-06 23:59:59.999999Z]]>
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
        %Tempus.Slot{from: nil, to: ~U[2020-08-06 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-08 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-13 00:00:00.000000Z], to: ~U[2020-08-13 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-15 00:00:00.000000Z], to: ~U[2020-08-15 23:59:59.999999Z]}
      ]
  """
  def next_free(slots, opts \\ []) do
    {origin, _count, iterator} = options(opts)
    {first, last} = {AVLTree.get_first(slots.slots), AVLTree.get_last(slots.slots)}

    slots =
      cond do
        DateTime.compare(first.from, origin.to) == :gt ->
          Slots.add(
            slots,
            Slot.shift(%Slot{from: origin.from, to: origin.from}, from: -1, to: -1)
          )

        DateTime.compare(origin.from, last.to) == :gt ->
          Slots.add(slots, Slot.shift(%Slot{from: origin.to, to: origin.to}, from: 1, to: 1))

        true ->
          slots
      end
      |> Slots.inverse()
      |> next_busy(opts)
      |> List.wrap()

    prev = Slot.shift(%Slot{to: first.from}, to: -1)
    next = Slot.shift(%Slot{from: last.to}, from: 1)

    case {iterator, slots} do
      {_, [%Slot{} = slot]} -> slot
      {-1, []} -> prev
      {1, []} -> next
      {-1, slots} when is_list(slots) -> [prev | Enum.reverse(slots)]
      {1, slots} when is_list(slots) -> slots ++ [next]
    end
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
  def add(slots, origin \\ DateTime.utc_now(), amount_to_add, unit \\ :second) do
    amount_in_μs = System.convert_time_unit(amount_to_add, unit, :microsecond)
  end

  @spec options(opts :: options()) :: options_tuple()
  defp options(opts) when is_list(opts) do
    origin =
      opts
      |> Keyword.get(:origin, DateTime.utc_now())
      |> Slot.wrap()

    count = Keyword.get(opts, :count, 0)

    unless is_integer(count),
      do: raise(Tempus.ArgumentError, expected: Integer, passed: count)

    iterator =
      if :erlang.xor(count < 0, Keyword.get(opts, :direction, :fwd) == :bwd), do: -1, else: 1

    {origin, abs(count), iterator}
  end
end
