defmodule Tempus.Slots do
  @moduledoc """
  The protocol to deal with the implementation of `Tempus.Slot` ordered collection.

  Comes with four default implementations, backed up by ordered list of slots,
    by `Stream`, by `AVLTree`, and by Year-Month tree.

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

  import Tempus.Guards

  @type container :: Enumerable.t(Slot.t())
  @type t(container) :: %Slots{slots: container}
  @type t() :: t(container())

  @implementation Application.compile_env(:tempus, :implementation, Tempus.Slots.List)
  defprotocol Group do
    @moduledoc """
    The protocol to implement for the ordered collection of slots.
    """

    alias Tempus.{Slot, Slots}

    @spec flatten(Slots.container()) :: {:ok, [Slot.t()]} | {:error, module()}
    def flatten(slots)
    @spec next(Slots.container(), Slot.origin()) :: {:ok, Slot.t()} | {:error, module()}
    def next(slots, origin)
    @spec previous(Slots.container(), Slot.origin()) :: {:ok, Slot.t()} | {:error, module()}
    def previous(slots, origin)

    @spec merge(Slots.container(), Slots.container(), keyword()) ::
            {:ok, Slots.container()} | {:error, module()}
    def merge(slots, other, options)
    @spec inverse(Slots.container()) :: {:ok, Slots.container()} | {:error, module()}
    def inverse(slots)
  end

  defstruct slots: struct!(@implementation, [])

  def new(implementation \\ @implementation),
    do: %Tempus.Slots{slots: struct!(implementation, [])}

  @spec size(t()) :: non_neg_integer()
  @doc deprecated: "Use `count/1` instead"
  @doc "Returns the number of slots"
  def size(%Slots{} = slots), do: count(slots)

  @spec count(t()) :: non_neg_integer()
  @doc "Returns the number of slots"
  def count(%Slots{} = slots), do: Enum.count(slots)

  @spec merge(slots :: [t()], keyword()) :: t()
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
  @telemetria level: :info
  def merge(slots, options \\ [])
  def merge([], _options), do: %Slots.Void{}
  def merge([%Slots{} = slots], _options), do: slots

  def merge([%Slots{slots: %_{} = head}, %Slots{slots: next} | rest], options) do
    case Group.merge(head, next, options) do
      {:ok, merged} -> merge([%Slots{slots: merged} | rest], options)
      {:error, _} -> raise "Not yet implemented"
    end
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
  # TODO maybe try to intelligently join neighbours
  @telemetria level: :debug
  def add(%Slots{} = slots, slot, join_neighbours \\ false) do
    merge([slots, %Tempus.Slots{slots: Tempus.Slots.List.wrap(slot)}],
      join_neighbours: join_neighbours
    )
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

  def inverse(slots, _options) do
    case Group.inverse(slots) do
      {:ok, inversed} -> inversed
      {:error, _} -> raise "Not yet implemented"
    end
  end

  @spec wrap(Slot.t() | [Slot.t()], impl :: module()) :: Slots.t()
  @doc since: "0.3.0"
  @doc """
  Wraps the argument into a slots instance. For `nil` it’d be an empty slots.
  For everything else it’d call `Slot.wrap/1` on an argument and add it to empty slots.

  ## Examples

      iex> Tempus.Slots.wrap(~D|2020-08-06|)
      #Slots<[#Slot<[from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]]>]>
  """
  def wrap(any, impl \\ @implementation), do: %Slots{slots: impl.wrap(any)}

  @spec less(s1 :: Slot.t(), s2 :: Slot.t()) :: boolean()
  @doc false
  def less(%Slot{} = s1, %Slot{} = s2) when is_coming_before(s1, s2), do: true
  def less(_, _), do: false

  defimpl Enumerable do
    @moduledoc false
    def reduce(_slots, {:halt, acc}, _fun), do: {:halted, acc}

    def reduce(%Slots{} = slots, {:suspend, acc}, fun),
      do: {:suspended, acc, &reduce(slots, &1, fun)}

    def reduce(%Slots{slots: []}, {:cont, acc}, _fun), do: {:done, acc}

    def reduce(%Slots{slots: [head | tail]}, {:cont, acc}, fun),
      do: reduce(%Slots{slots: tail}, fun.(head, acc), fun)

    def member?(%Slots{slots: slots}, value), do: Enumerable.member?(slots, value)
    def count(%Slots{slots: slots}), do: Enumerable.count(slots)
    def slice(%Slots{slots: slots}), do: Enumerable.slice(slots)
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
