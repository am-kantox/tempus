defmodule Tempus.Slots do
  @moduledoc """
  The interface to deal with the implementation of `Tempus.Slot` ordered collection.

  Comes with several default implementations, backed up by ordered list of slots,
    by `Stream`, by `AVLTree`¹, and by Year-Month² tree.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}]
      ...> Enum.into(slots, %Tempus.Slots{})
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]}}
      iex> Enum.map(slots, & &1.from)
      [~U[2020-08-07 00:00:00.000000Z], ~U[2020-08-10 00:00:00.000000Z], ~U[2020-08-07 01:00:00Z]]
  """

  alias Tempus.{Slot, Slots}

  @type container :: Enumerable.t(Slot.t())
  @type t(container) :: %Slots{slots: container}
  @type t() :: t(container())

  defprotocol Group do
    @moduledoc """
    The protocol to implement for the ordered collection of slots.
    """

    alias Tempus.{Slot, Slots}

    @spec flatten(Slots.container()) :: {:ok, [Slot.t()]} | {:error, module()}
    def flatten(slots)

    @spec next(Slots.container(), Slot.origin(), non_neg_integer()) ::
            {:ok, Slot.t()} | {:error, module()}
    def next(slots, origin, count)

    @spec previous(Slots.container(), Slot.origin(), non_neg_integer()) ::
            {:ok, Slot.t()} | {:error, module()}
    def previous(slots, origin, count)

    @spec merge(Slots.container(), Slots.container(), keyword()) ::
            {:ok, Slots.container()} | {:error, module()}
    def merge(slots, other, options)
    @spec inverse(Slots.container()) :: {:ok, Slots.container()} | {:error, module()}
    def inverse(slots)
  end

  @implementation Application.compile_env(:tempus, :implementation, Tempus.Slots.List)

  defstruct slots: struct!(@implementation, [])

  @doc """
  Creates an instance of slots, using a backend given as a parameter.
  """
  def new(implementation \\ @implementation)
  def new(:list), do: new(Slots.List)
  def new(:stream), do: new(Slots.Stream)
  def new(implementation), do: %Slots{slots: implementation.new()}

  @spec size(t()) :: non_neg_integer()
  @doc deprecated: "Use `count/1` instead"
  @doc "Returns the number of slots"
  def size(%Slots{} = slots), do: count(slots)

  @spec count(t()) :: non_neg_integer()
  @doc "Returns the number of slots"
  def count(%Slots{} = slots), do: Enum.count(slots)

  @spec merge(slots :: t() | [t()], Enumerable.t(Slot.t()) | keyword(), keyword()) :: t()
  @doc """
  Merges many slots into the first element in the list given as an argument.

  Other arguments might be streams, the first one must be a `Tempus.Slots` instance.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots{})
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]}}
      iex> other = [
      ...>   %Tempus.Slot{from: ~U|2020-08-07 23:00:00Z|, to: ~U|2020-08-08 12:00:00Z|},
      ...>   %Tempus.Slot{from: ~U|2020-08-12 23:00:00Z|, to: ~U|2020-08-12 23:30:00Z|}
      ...> ]
      iex> Tempus.Slots.merge([slots, other])
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]}]}}
      iex> Tempus.Slots.merge([slots, Tempus.Slots.wrap(other, Tempus.Slots.Stream)]) |> Enum.to_list()
      [%Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]},
       %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]},
       %Tempus.Slot{from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]}]

  """
  def merge(slots, enum \\ [], options \\ [])
  def merge(%Slots{} = slots, enum, options), do: do_merge([slots, enum], options)
  def merge(slots, options, []), do: do_merge(slots, options)

  defp do_merge([], _options), do: %Slots.Void{}
  defp do_merge([%Slots{} = slots], _options), do: slots

  defp do_merge([%Slots{slots: %_{} = head}, %Slots{slots: %_{} = next} | rest], options) do
    case Group.merge(head, next, options) do
      {:ok, merged} -> merge([%Slots{slots: merged} | rest], options)
      {:error, _} -> raise "Not yet implemented"
    end
  end

  defp do_merge([%Slots{slots: %implementation{}} = head, next | rest], options)
       when is_list(next) or is_struct(next, Stream) or is_function(next, 2),
       do: merge([head, %Slots{slots: implementation.wrap(next)} | rest], options)

  @spec add(slots :: t(), slot :: Slot.origin()) :: t()
  @doc """
  Adds another slot to the slots collection.

  Joins slots intersecting with the new one, if any.

  ### Example

      iex> Tempus.Slots.add(%Tempus.Slots{}, Tempus.Slot.wrap(~D|2020-08-07|))
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}]}}

      iex> %Tempus.Slots{}
      ...> |> Tempus.Slots.add(Tempus.Slot.wrap(~D|2020-08-07|))
      ...> |> Tempus.Slots.add(Tempus.Slot.wrap(~D|2020-08-10|))
      ...> |> Tempus.Slots.add(%Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|})
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]}}
  """
  # TODO maybe try to intelligently join neighbours
  def add(%Slots{} = slots, slot) do
    merge([slots, %Tempus.Slots{slots: Tempus.Slots.List.wrap(slot)}])
  end

  @spec inverse(slots :: Slots.t()) :: Slots.t()
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
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: nil, to: ~U[2020-08-06 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-13 00:00:00.000000Z], to: nil}]}}

      iex> [
      ...>   %Tempus.Slot{to: ~U[2020-08-08 23:59:59.999999Z]},
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{from: ~U[2020-08-12 00:00:00.000000Z]}
      ...> ] |> Enum.into(%Tempus.Slots{})
      ...> |> Tempus.Slots.inverse()
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]}]}}
  """
  def inverse(slots, options \\ [])

  def inverse(%Slots{slots: slots}, _options) do
    case Group.inverse(slots) do
      {:ok, inversed} -> %Slots{slots: inversed}
      {:error, _} -> raise "Not yet implemented"
    end
  end

  @spec wrap([Slot.origin()] | Slots.t(), module()) :: Slots.t()
  @doc since: "0.3.0"
  @doc """
  Wraps the argument into a slots instance. For `nil` it’d be an empty slots.
  For everything else it’d call `Slot.wrap/1` on an argument and add it to empty slots.

  ## Examples

      iex> Tempus.Slots.wrap(~D|2020-08-06|)
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]}]}}
  """
  def wrap(any, implementation \\ @implementation)
  def wrap(%Slots{slots: %implementation{}} = slots, implementation), do: slots

  def wrap(%Slots{slots: slots}, implementation) do
    with {:ok, slots} <- Group.flatten(slots), do: wrap(slots, implementation)
  end

  def wrap(slots, implementation), do: %Slots{slots: implementation.wrap(slots)}

  defimpl Enumerable do
    @moduledoc false
    def reduce(%Slots{slots: %_{} = slots}, acc, fun),
      do: Enumerable.reduce(slots, acc, fun)

    def member?(%Slots{slots: _slots}, _value), do: {:error, __MODULE__}
    def count(%Slots{slots: _slots}), do: {:error, __MODULE__}
    def slice(%Slots{slots: _slots}), do: {:error, __MODULE__}
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
            Enum.slice(slots.slots, 0, 1) ++
              [
                "… ‹#{Enum.count(slots) - 2 - i} more› …" | Enum.slice(slots.slots, -i - 1, i + 1)
              ]

          _ ->
            slots
        end

      concat(["#Slots<", to_doc(inner_doc, opts), ">"])
    end
  end
end
