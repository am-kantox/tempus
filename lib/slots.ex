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

  alias Tempus.{Slot, Slots, Slots.Group}

  import Tempus.Guards

  @type container :: Enumerable.t(Slot.t())
  @opaque implementation(group) :: %{__struct__: group, slots: container()}
  @type t(group) :: %Slots{slots: implementation(group)}
  @type t() :: t(Slots.List)

  @implementation Application.compile_env(:tempus, :implementation, Tempus.Slots.List)

  defstruct slots: struct!(@implementation, [])

  @doc """
  Creates an instance of slots, using a backend given as a parameter.
  """
  def new(implementation \\ @implementation, data)
  def new(@implementation, %implementation{} = data), do: new(implementation, data)
  def new(:list, data), do: new(Slots.List, data)
  def new(:stream, data), do: new(Slots.Stream, data)
  def new(implementation, data), do: %Slots{slots: Enum.into(data, implementation.new())}

  @spec size(t()) :: non_neg_integer()
  @doc deprecated: "Use `count/1` instead"
  @doc false
  def size(%Slots{} = slots), do: count(slots)

  @spec count(t()) :: non_neg_integer()
  @doc "Returns the number of slots"
  def count(%Slots{} = slots), do: Enum.count(slots)

  @spec identity(t()) :: container()
  @doc false
  def identity(%Slots{slots: slots}), do: %Slots{slots: Group.identity(slots)}

  @spec flatten(t(), keyword()) :: [Slot.t()]
  def flatten(%Slots{slots: slots}, options \\ []), do: Group.flatten(slots, options)

  @spec drop_until(t(), Slot.origin(), keyword()) :: t()
  def drop_until(%Slots{slots: %implementation{} = slots}, origin, options \\ []) do
    case Group.drop_until(slots, origin, options) do
      {:ok, slots} ->
        %Slots{slots: slots}

      {:error, _} ->
        slots
        |> Group.flatten(options)
        |> Enum.drop_while(&is_coming_before(&1, origin))
        |> wrap(Keyword.put(options, :implementation, implementation))
    end
  end

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
  def add(%Slots{slots: slots}, slot, options \\ []),
    do: %Slots{slots: Group.add(slots, slot, options)}

  @spec merge(slots :: t() | [t()], Enumerable.t(Slot.t()) | keyword(), keyword()) :: t()
  @doc """
  Merges many slots into the first element in the list given as an argument.

  Other arguments might be enumerables, the first one must be a `Tempus.Slots` instance.

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

  defp do_merge([%Slots{slots: %_{} = head} = slots, %Slots{slots: %_{} = next} | rest], options) do
    case Group.merge(head, next, options) do
      {:ok, merged} -> merge([%Slots{slots: merged} | rest], options)
      {:error, _} -> do_merge([slots, Group.flatten(next, options) | rest], options)
    end
  end

  defp do_merge([%Slots{slots: %_{} = head}, next | rest], options) when is_list(next) do
    next = Enum.reduce(next, head, &Group.add(&2, &1, options))
    merge([%Slots{slots: next} | rest], options)
  end

  @spec inverse(slots :: Slots.t(), keyword()) :: Slots.t()
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

  def inverse(%Slots{slots: slots}, options) do
    case Group.inverse(slots, options) do
      {:ok, inversed} -> %Slots{slots: inversed}
      {:error, _} -> raise "Not yet implemented"
    end
  end

  @spec wrap([Slot.origin()] | Slots.t(), keyword()) :: Slots.t()
  @doc since: "0.3.0"
  @doc """
  Wraps the argument into a slots instance. For `nil` it’d be an empty slots.
  For everything else it’d call `Slot.wrap/1` on an argument and add it to empty slots.

  ## Examples

      iex> Tempus.Slots.wrap(~D|2020-08-06|)
      %Tempus.Slots{slots: %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]}]}}
  """
  def wrap(any, options \\ [])

  def wrap(any, implementation) when is_atom(implementation),
    do: do_wrap(any, implementation, [])

  def wrap(any, options) when is_list(options) do
    {implementation, options} = Keyword.pop(options, :implementation, @implementation)
    do_wrap(any, implementation, options)
  end

  defp do_wrap(%Slots{slots: %implementation{}} = slots, implementation, _options), do: slots

  defp do_wrap(%Slots{slots: slots}, implementation, options) do
    slots |> Group.flatten(options) |> do_wrap(implementation, options)
  end

  defp do_wrap(slots, implementation, options) do
    merge(%Slots{slots: implementation.new()}, List.wrap(slots), options)
  end

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
