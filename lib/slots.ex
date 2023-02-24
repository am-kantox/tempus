defmodule Tempus.Slots do
  @moduledoc """
  The ordered collection of slots, backed up by `AVLTree`.

  This module implements `Enumerable` and `Collectable` interfaces.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}]
      ...> Enum.into(slots, %Tempus.Slots{})
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>]>
      iex> Enum.map(slots, & &1.from)
      [~U[2020-08-07 00:00:00.000000Z], ~U[2020-08-10 00:00:00.000000Z], ~U[2020-08-07 01:00:00Z]]
  """
  alias Tempus.{Slot, Slots}

  use Tempus.Telemetria, action: :import

  @empty AVLTree.new(&Slots.less/2)

  defstruct slots: []

  @typedoc "AVL Tree specialized for `Tempus` slots type"
  @type avl_tree :: %AVLTree{
          root: nil | Slot.t(),
          size: non_neg_integer(),
          less: (Slot.t(), Slot.t() -> boolean())
        }

  @type t :: %Slots{slots: [Slot.t()]}

  @spec size(t()) :: integer()
  @doc "Returns the number of slots"
  def size(%Slots{slots: slots}), do: length(slots)

  @spec avl_tree(t()) :: avl_tree()
  @doc "Returns the AVL Tree instance of slots"
  def avl_tree(%Slots{slots: slots}), do: Enum.into(slots, @empty)

  @spec merge(slots :: [t()]) :: t()
  @doc """
  Merges many slots into the first element in the list given as an argument.

  Other arguments might be streams, the first one must be a `Tempus.Slots` instance.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> other = [
      ...>   %Tempus.Slot{from: ~U|2020-08-07 23:00:00Z|, to: ~U|2020-08-08 12:00:00Z|},
      ...>   %Tempus.Slot{from: ~U|2020-08-12 23:00:00Z|, to: ~U|2020-08-12 23:30:00Z|}
      ...> ]
      iex> Tempus.Slots.merge([slots, other])
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]]>]>
      iex> Tempus.Slots.merge([slots, Stream.map(other, & &1)])
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]]>]>

  """
  def merge([]), do: %Slots{}

  def merge([%Slots{} | _] = slots) when is_list(slots),
    do: Enum.reduce(slots, &merge(&2, &1))

  @spec merge(this :: t(), other :: Enumerable.t()) :: t()
  @doc """
  Merges `other` into `this` slots instance. `other` might be `Enum` _or_ `Stream`.
  When `other` is a stream, it gets terminated immediately after the last element
  in `this`.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      iex> other = [
      ...>   %Tempus.Slot{from: ~U|2020-08-07 23:00:00Z|, to: ~U|2020-08-08 12:00:00Z|},
      ...>   %Tempus.Slot{from: ~U|2020-08-12 23:00:00Z|, to: ~U|2020-08-12 23:30:00Z|}
      ...> ]
      iex> Tempus.Slots.merge(slots, other)
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]]>]>
      iex> Tempus.Slots.merge(slots, Stream.map(other, & &1))
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]]>]>

  """
  @telemetria level: :info
  def merge(this, other)

  def merge(%Slots{} = this, %Stream{} = other),
    do: do_merge_stream(this, other)

  def merge(%Slots{} = this, other) when is_function(other),
    do: do_merge_stream(this, other)

  def merge(%Slots{} = this, %Slot{} = slot),
    do: add(this, slot)

  def merge(%Slots{} = this, other) do
    if is_nil(Enumerable.impl_for(other)) do
      raise Tempus.ArgumentError, expected: Enum, passed: other
    end

    Enum.reduce(other, this, &add(&2, &1))
  end

  @spec do_merge_stream(this :: t(), other :: Enumerable.t()) :: t()
  defp do_merge_stream(%Slots{slots: []}, other),
    do: %Slots{slots: Enum.take(other, 1)}

  defp do_merge_stream(%Slots{slots: slots}, other) do
    chunk_fun = fn slot, acc ->
      slot = Slot.wrap(slot)
      {head, tail} = Enum.split_while(acc, &(less(&1, slot) and not Slot.neighbour?(&1, slot)))

      {joint, tail} =
        Enum.split_while(
          tail,
          &(Slot.strict_compare(&1, slot) != :gt or Slot.neighbour?(&1, slot))
        )

      %Slots{slots: joint} = add(wrap_unsafe(joint), slot, true)

      case tail do
        [] -> {:halt, head ++ joint}
        _ -> {:cont, head ++ joint, tail}
      end
    end

    after_fun = fn
      [] -> {:cont, []}
      acc -> {:cont, acc, []}
    end

    slots =
      other
      |> Stream.chunk_while(slots, chunk_fun, after_fun)
      |> Enum.flat_map(& &1)

    %Slots{slots: slots}
  end

  @spec add(slots :: t(), slot :: Slot.origin(), join_neighbours :: boolean()) :: t()
  @doc """
  Adds another slot to the slots collection.

  Joins slots intersecting with the new one, if any.

  ### Example

      iex> Tempus.Slots.add(%Tempus.Slots{}, Tempus.Slot.wrap(~D|2020-08-07|))
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>]>

      iex> %Tempus.Slots{}
      ...> |> Tempus.Slots.add(Tempus.Slot.wrap(~D|2020-08-07|))
      ...> |> Tempus.Slots.add(Tempus.Slot.wrap(~D|2020-08-10|))
      ...> |> Tempus.Slots.add(%Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|})
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>]>
  """
  @telemetria level: :debug
  def add(slots, slot, join_neighbours \\ false)

  def add(%Slots{slots: []}, slot, _),
    do: %Slots{slots: [Slot.wrap(slot)]}

  def add(%Slots{slots: slots}, slot, false) do
    slot = Slot.wrap(slot)

    case Enum.split_while(slots, &(Slot.strict_compare(&1, slot) == :lt)) do
      {^slots, []} ->
        %Slots{slots: slots ++ [slot]}

      {head, slots} ->
        tail =
          case Enum.split_while(slots, &(Slot.strict_compare(&1, slot) != :gt)) do
            {[], ^slots} ->
              [slot | slots]

            {joint, tail} ->
              [Enum.reduce(joint, slot, &Slot.join/2) | tail]
          end

        %Slots{slots: head ++ tail}
    end
  end

  def add(%Slots{slots: slots}, slot, true) do
    slot = Slot.wrap(slot)

    case Enum.split_while(
           slots,
           &(Slot.strict_compare(&1, slot) == :lt and not Slot.neighbour?(&1, slot))
         ) do
      {^slots, []} ->
        %Slots{slots: slots ++ [slot]}

      {head, slots} ->
        tail =
          case Enum.split_while(
                 slots,
                 &(Slot.strict_compare(&1, slot) != :gt or Slot.neighbour?(&1, slot))
               ) do
            {[], ^slots} ->
              [slot | slots]

            {joint, tail} ->
              [Enum.reduce(joint, slot, &Slot.join/2) | tail]
          end

        %Slots{slots: head ++ tail}
    end
  end

  @spec inverse(slots :: Slots.t(), tails :: :keep | :discard) :: Slots.t()
  @doc """
  Inverses `Slots` returning the new `Slots` instance with slots set where
    there were blanks.

  ### Example

      iex> [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-08|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   Tempus.Slot.wrap(~D|2020-08-12|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      ...> |> Tempus.Slots.inverse()
      %Tempus.Slots{slots: [
        %Tempus.Slot{from: nil, to: ~U[2020-08-06 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-13 00:00:00.000000Z], to: nil}]}

      iex> [
      ...>   %Tempus.Slot{to: ~U[2020-08-08 23:59:59.999999Z]},
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{from: ~U[2020-08-12 00:00:00.000000Z]}
      ...> ] |> Enum.into(%Tempus.Slots{})
      ...> |> Tempus.Slots.inverse()
      %Tempus.Slots{slots: [
        %Tempus.Slot{from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]}
      ]}
  """
  @telemetria level: :info
  def inverse(slots, tails \\ :keep)

  def inverse(%Slots{slots: []} = slots, _), do: slots

  def inverse(%Slots{slots: slots}, tails) do
    tail =
      slots
      |> Enum.chunk_every(2, 1)
      |> Enum.reduce([], fn
        [%Slot{to: from}, %Slot{from: to}], acc ->
          slot = Slot.shift(%Slot{from: from, to: to}, from: 1, to: -1)
          if Slot.valid?(slot), do: [slot | acc], else: acc

        [%Slot{to: from}], acc ->
          if tails == :keep and not is_nil(from),
            do: [Slot.shift(%Slot{from: from}, from: 1) | acc],
            else: acc
      end)
      |> Enum.sort({:asc, Slot})

    slots =
      if tails == :keep and not is_nil(hd(slots).from),
        do: [Slot.shift(%Slot{to: hd(slots).from}, to: -1) | tail],
        else: tail

    %Slots{slots: slots}
  end

  @spec wrap(Slot.t() | [Slot.t()]) :: Slots.t()
  @doc since: "0.3.0"
  @doc """
  Wraps the argument into a slots instance. For `nil` it’d be an empty slots.
  For everything else it’d call `Slot.wrap/1` on an argument and add it to empty slots.

  ## Examples

      iex> Tempus.Slots.wrap(~D|2020-08-06|)
      #Slots<[#Slot<[from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]]>]>
  """
  def wrap(nil), do: %Slots{}
  def wrap(slots) when is_list(slots), do: %Slots{slots: Enum.map(slots, &Slot.wrap/1)}
  def wrap(slot), do: Slots.add(%Slots{}, Slot.wrap(slot))

  @spec wrap_unsafe([Slot.t()]) :: Slots.t()
  @doc false
  def wrap_unsafe(slots) when is_list(slots), do: %Slots{slots: slots}

  @spec less(s1 :: Slot.t(), s2 :: Slot.t()) :: boolean()
  @doc false
  def less(%Slot{} = s1, %Slot{} = s2),
    do: Slot.strict_compare(s1, s2) == :lt

  defimpl Enumerable do
    @moduledoc false
    def reduce(_slots, {:halt, acc}, _fun), do: {:halted, acc}

    def reduce(%Slots{} = slots, {:suspend, acc}, fun),
      do: {:suspended, acc, &reduce(slots, &1, fun)}

    def reduce(%Slots{slots: []}, {:cont, acc}, _fun), do: {:done, acc}

    def reduce(%Slots{slots: [head | tail]}, {:cont, acc}, fun),
      do: reduce(%Slots{slots: tail}, fun.(head, acc), fun)

    def member?(%Slots{slots: slots}, value),
      do: Enumerable.member?(slots, value)

    def count(%Slots{slots: %AVLTree{size: size}}), do: {:ok, size}
    def count(%Slots{slots: slots}), do: {:ok, length(slots)}
    def slice(%Slots{slots: slots}), do: {:ok, length(slots), & &1.slots}
  end

  defimpl Collectable do
    @moduledoc false
    alias Tempus.Slots

    def into(original) do
      {
        original,
        fn
          slots, {:cont, value} -> Slots.add(slots, value)
          slots, :done -> slots
          _, :halt -> :ok
        end
      }
    end
  end

  defimpl Inspect do
    @moduledoc false
    import Inspect.Algebra

    def inspect(%Tempus.Slots{slots: slots}, opts) do
      inner_doc =
        opts.custom_options
        |> Keyword.get(:truncate, false)
        |> case do
          false -> false
          true -> 0
          i when is_integer(i) and i < length(slots) - 2 -> i
          _ -> false
        end
        |> case do
          i when is_integer(i) and i >= 0 ->
            [hd(slots), "… ‹#{length(slots) - 2 - i} more› …" | Enum.slice(slots, -i - 1, i + 1)]

          _ ->
            slots
        end

      concat(["#Slots<", to_doc(inner_doc, opts), ">"])
    end
  end
end
