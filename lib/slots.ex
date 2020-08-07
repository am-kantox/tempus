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

  # @spec merge(slots :: [Slot.t()]) :: [Slot.t()]
  # def merge(slots) when is_list(slots) do
  # end

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

  @spec less(s1 :: Slot.t(), s2 :: Slot.t()) :: boolean()
  def less(%Slot{to: to}, %Slot{from: from}),
    do: DateTime.compare(from, to) == :gt

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
