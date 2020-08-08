defmodule Tempus.Slots do
  @moduledoc """
  The ordered collection of slots, backed up by `AVLTree`.

  This module implements `Enumerable` and `Collectable` interfaces.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.day(~D|2020-08-07|),
      ...>   Tempus.Slot.day(~D|2020-08-10|),
      ...>   %Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}]
      ...> Enum.into(slots, %Tempus.Slots{})
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>]>
      iex> Enum.map(slots, & &1.from)
      [~U[2020-08-07 00:00:00.000000Z], ~U[2020-08-10 00:00:00.000000Z], ~U[2020-08-07 01:00:00Z]]
  """
  alias Tempus.{Slot, Slots}

  @typedoc "AVL Tree specialized for `Tempus` slots type"
  @type avl_tree :: %AVLTree{
          root: nil | Slot.t(),
          size: non_neg_integer(),
          less: (Slot.t(), Slot.t() -> boolean())
        }

  @type t :: %Slots{slots: avl_tree()}

  @empty AVLTree.new(&Slots.less/2)

  defstruct slots: @empty

  @spec merge(this :: t(), other :: Enum.t()) :: t()
  @doc """
  Merges `other` into `this` slots instance. `other` might be `Enum` _or_ `Stream`.
  When `other` is a stream, it gets terminated immediately after the last element
  in `this`.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.day(~D|2020-08-07|),
      ...>   Tempus.Slot.day(~D|2020-08-10|)
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
  def merge(%Slots{slots: slots} = this, %Stream{} = other) do
    case AVLTree.get_last(slots) do
      nil ->
        other
        |> Enum.take(1)
        |> case do
          [] -> this
          [%Slot{} = slot] -> add(this, slot)
          [other] -> raise Tempus.ArgumentError, expected: Tempus.Slot, passed: other
        end

      %Slot{} = last ->
        other
        |> Stream.take_while(fn
          %Slot{} = slot -> less(slot, last)
          other -> raise Tempus.ArgumentError, expected: Tempus.Slot, passed: other
        end)
        |> Enum.reduce(this, &add(&2, &1))
    end
  end

  def merge(%Slots{} = this, %Slot{} = slot),
    do: add(this, slot)

  def merge(%Slots{} = this, other) do
    if is_nil(Enumerable.impl_for(other)) do
      raise Tempus.ArgumentError, expected: Enum, passed: other
    end

    Enum.reduce(other, this, &add(&2, &1))
  end

  @spec add(t(), Slot.t()) :: t()
  @doc """
  Adds another slot to the slots collection.

  Joins slots intersecting with the new one, if any.

  ### Example

      iex> Tempus.Slots.add(%Tempus.Slots{}, Tempus.Slot.day(~D|2020-08-07|))
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>]>

      iex> %Tempus.Slots{}
      ...> |> Tempus.Slots.add(Tempus.Slot.day(~D|2020-08-07|))
      ...> |> Tempus.Slots.add(Tempus.Slot.day(~D|2020-08-10|))
      ...> |> Tempus.Slots.add(%Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|})
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>]>
  """
  def add(%Slots{slots: slots}, %Slot{} = slot) do
    {slots, joint} = Enum.split_with(slots, &Slot.disjoint?(&1, slot))

    %Slots{
      slots:
        slots
        |> Enum.into(@empty)
        |> AVLTree.put(Enum.reduce(joint, slot, &Slot.join([&1, &2])))
    }
  end

  def add(%Slots{}, other),
    do: raise(Tempus.ArgumentError, expected: Tempus.Slot, passed: other)

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
