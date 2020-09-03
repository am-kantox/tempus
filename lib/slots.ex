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

  @empty AVLTree.new(&Slots.less/2)

  defstruct slots: @empty

  @typedoc "AVL Tree specialized for `Tempus` slots type"
  @type avl_tree :: %AVLTree{
          root: nil | Slot.t(),
          size: non_neg_integer(),
          less: (Slot.t(), Slot.t() -> boolean())
        }

  @type t :: %Slots{slots: avl_tree()}

  @spec size(t()) :: integer()
  @doc "Returns the number of slots"
  def size(%Slots{slots: slots}), do: AVLTree.size(slots)

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
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>]>

  """
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
  defp do_merge_stream(%Slots{slots: slots} = this, other) do
    case AVLTree.get_last(slots) do
      nil ->
        other
        |> Enum.take(1)
        |> case do
          [] -> this
          [slot] -> add(this, Slot.wrap(slot))
        end

      %Slot{} = last ->
        other
        |> Stream.take_while(&(&1 |> Slot.wrap() |> less(last)))
        |> Enum.reduce(this, &add(&2, &1))
    end
  end

  @spec add(t(), Slot.origin()) :: t() | no_return
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
  def add(%Slots{slots: slots}, slot) do
    slot = Slot.wrap(slot)
    {slots, joint} = Enum.split_with(slots, &Slot.disjoint?(&1, slot))

    %Slots{
      slots:
        slots
        |> Enum.into(@empty)
        |> AVLTree.put(Enum.reduce(joint, slot, &Slot.join([&1, &2])))
    }
  end

  @spec inverse(slots :: Slots.t(), remainder :: :keep | :discard) :: Slots.t()
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
      #Slots<[#Slot<[from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]]>]>
  """
  def inverse(%Slots{} = slots, remainder \\ :discard) do
    result =
      Enum.reduce(slots, {nil, %Slots{}}, fn
        %Slot{} = slot, {nil, slots} ->
          {slot, slots}

        %Slot{} = this, {prev, slots} ->
          if DateTime.diff(this.from, prev.to, :microsecond) <= 100,
            do: {%Slot{prev | to: this.to}, slots},
            else:
              {this,
               Slots.add(
                 slots,
                 Slot.shift(%Slot{from: prev.to, to: this.from}, from: 1, to: -1)
               )}
      end)

    if remainder == :keep, do: result, else: elem(result, 1)
  end

  @spec less(s1 :: Slot.t(), s2 :: Slot.t()) :: boolean()
  @doc false
  def less(%Slot{} = s1, %Slot{} = s2),
    do: Slot.strict_compare(s1, s2) == :lt

  defimpl Enumerable do
    @moduledoc false
    def reduce(%Slots{slots: slots}, {state, acc}, fun),
      do: Enumerable.reduce(slots, {state, acc}, fun)

    def member?(%Slots{slots: slots}, value),
      do: Enumerable.member?(slots, value)

    def count(%Slots{slots: %AVLTree{size: size}}), do: {:ok, size}

    def slice(_) do
      {:error, __MODULE__}
    end
  end

  defimpl Collectable do
    @moduledoc false
    def into(original) do
      {
        original,
        fn
          slots, {:cont, value} -> Tempus.Slots.add(slots, value)
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
      concat(["#Slots<", to_doc(Enum.to_list(slots), opts), ">"])
    end
  end
end
