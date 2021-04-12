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
  @telemetria level: :info
  def merge(%Slots{} = this, %Stream{} = other),
    do: do_merge_stream(this, other)

  @telemetria level: :info
  def merge(%Slots{} = this, other) when is_function(other),
    do: do_merge_stream(this, other)

  @telemetria level: :info
  def merge(%Slots{} = this, %Slot{} = slot),
    do: add(this, slot)

  @telemetria level: :info
  def merge(%Slots{} = this, other) do
    if is_nil(Enumerable.impl_for(other)) do
      raise Tempus.ArgumentError, expected: Enum, passed: other
    end

    Enum.reduce(other, this, &add(&2, &1))
  end

  @spec do_merge_stream(this :: t(), other :: Enumerable.t()) :: t()
  defp do_merge_stream(%Slots{slots: []}, other),
    do: %Slots{slots: Enum.take(other, 1)}

  defp do_merge_stream(%Slots{slots: slots} = this, other) do
    other =
      other
      |> Stream.take_while(&(&1 |> Slot.wrap() |> less(List.last(slots))))
      |> Enum.to_list()

    merge(this, other)
  end

  @spec add(t(), Slot.origin()) :: t()
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
  def add(%Slots{slots: []}, slot),
    do: %Slots{slots: [Slot.wrap(slot)]}

  @telemetria level: :debug
  def add(%Slots{slots: slots}, slot) do
    slot = Slot.wrap(slot)

    case Enum.split_with(slots, &(Slot.strict_compare(&1, slot) == :lt)) do
      {^slots, []} ->
        %Slots{slots: slots ++ [slot]}

      {head, slots} ->
        tail =
          case Enum.split_with(slots, &(Slot.strict_compare(&1, slot) == :gt)) do
            {^slots, []} ->
              [slot | slots]

            {tail, joint} ->
              [Enum.reduce(joint, slot, &Slot.join([&1, &2])) | tail]
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
  def inverse(slots, tails \\ :keep)

  @telemetria level: :info
  def inverse(%Slots{slots: []} = slots, _), do: slots

  @telemetria level: :info
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

  @spec wrap(Slot.t()) :: Slots.t()
  @doc since: "0.3.0"
  @doc """
  Wraps the argument into a slots instance. For `nil` it’d be an empty slots.
  For everything else it’d call `Slot.wrap/1` on an argument and add it to empty slots.

  ## Examples

      iex> Tempus.Slots.wrap(~D|2020-08-06|)
      #Slots<[#Slot<[from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]]>]>
  """
  def wrap(nil), do: %Slots{}
  def wrap(slot), do: Slots.add(%Slots{}, Slot.wrap(slot))

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
      concat(["#Slots<", to_doc(Enum.to_list(slots), opts), ">"])
    end
  end
end
