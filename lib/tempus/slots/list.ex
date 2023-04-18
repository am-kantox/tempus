defmodule Tempus.Slots.List do
  @moduledoc """
  The default `List` implementation of `Tempus.Slots` ordered collection.

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
  import Tempus.Slot, only: [void: 0]

  defstruct slots: []

  @type t :: %{
          __struct__: __MODULE__,
          slots: [Slot.t()]
        }

  @doc """
  Adds another slot to the slots collection.

  Joins slots intersecting with the new one, if any.

  ### Example

      iex> Tempus.Slots.List.add(%Tempus.Slots.List{}, Tempus.Slot.wrap(~D|2020-08-07|))
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]]>]>

      iex> %Tempus.Slots.List{}
      ...> |> Tempus.Slots.List.add(Tempus.Slot.wrap(~D|2020-08-07|))
      ...> |> Tempus.Slots.List.add(Tempus.Slot.wrap(~D|2020-08-10|))
      ...> |> Tempus.Slots.List.add(%Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|})
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>]>
  """
  @spec add(Slots.List.t(), Slot.t(), keyword()) :: Slots.List.t()
  @telemetria level: :debug
  def add(%Slots.List{slots: slots}, %Slot{} = slot, _options \\ []),
    do: %Slots.List{slots: do_add(slot, [], slots)}

  defp do_add(slot, head, []), do: Enum.reverse([slot | head])

  defp do_add(slot, head, [th | tail]) when is_coming_before(th, slot),
    do: do_add(slot, [th | head], tail)

  defp do_add(slot, head, [th | tail]) when is_coming_before(slot, th),
    do: Enum.reverse(head) ++ [slot, th | tail]

  defp do_add(slot, head, [th | tail]),
    do: Enum.reverse(head) ++ [Slot.join(slot, th) | tail]

  @doc """
  Merges `other` into `this` slots instance. `other` might be `Enum` _or_ `Stream`.
  When `other` is a stream, it gets terminated immediately after the last element
  in `this`.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(%Tempus.Slots.List{})
      iex> other = [
      ...>   %Tempus.Slot{from: ~U|2020-08-07 23:00:00Z|, to: ~U|2020-08-08 12:00:00Z|},
      ...>   %Tempus.Slot{from: ~U|2020-08-12 23:00:00Z|, to: ~U|2020-08-12 23:30:00Z|}
      ...> ]
      iex> Tempus.Slots.List.merge(slots, other)
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]]>]>
      iex> Tempus.Slots.List.merge(slots, Stream.map(other, & &1))
      #Slots<[#Slot<[from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]]>, #Slot<[from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]]>, #Slot<[from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]]>]>

  """
  @spec merge(t(), Slots.t(), keyword()) :: t()
  @telemetria level: :info
  def merge(slots, other, options \\ [])

  def merge(%Slots.List{slots: slots}, %Slots.List{slots: other}, _options),
    do: %Slots.List{slots: do_merge_lists(slots, other, [])}

  def merge(%Slots.List{} = list, %Slots.Stream{} = stream, _options),
    do: Slots.Stream.merge(stream, list)

  def merge(%Slots.List{} = _list, _other, _options),
    do: raise(ArgumentError, message: "Merging different impls is not yet supported")

  defp do_merge_lists([], other, result), do: Enum.reverse(result) ++ other
  defp do_merge_lists(slots, [], result), do: Enum.reverse(result) ++ slots

  defp do_merge_lists([hs | ts], [ho | to], result) when is_coming_before(hs, ho),
    do: do_merge_lists(ts, [ho | to], [hs | result])

  defp do_merge_lists([hs | ts], [ho | to], result) when is_coming_before(ho, hs),
    do: do_merge_lists([hs | ts], to, [ho | result])

  defp do_merge_lists([hs | ts], [ho | to], result) when is_coming_before(hs.to, ho.to),
    do: do_merge_lists(ts, [Slot.join(hs, ho) | to], result)

  defp do_merge_lists([hs | ts], [ho | to], result) when is_coming_before(ho.to, hs.to),
    do: do_merge_lists([Slot.join(hs, ho) | ts], to, result)

  defp do_merge_lists([hs | ts], [ho | to], result),
    do: do_merge_lists(ts, to, [Slot.join(hs, ho) | result])

  @doc """
  Inverses `Slots` returning the new `Slots` instance with slots set where
    there were blanks.

  ### Example

      iex> [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-08|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   Tempus.Slot.wrap(~D|2020-08-12|)
      ...> ] |> Enum.into(%Tempus.Slots.List{})
      ...> |> Tempus.Slots.List.inverse()
      %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: nil, to: ~U[2020-08-06 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-13 00:00:00.000000Z], to: nil}]}

      iex> [
      ...>   %Tempus.Slot{to: ~U[2020-08-08 23:59:59.999999Z]},
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{from: ~U[2020-08-12 00:00:00.000000Z]}
      ...> ] |> Enum.into(%Tempus.Slots.List{})
      ...> |> Tempus.Slots.List.inverse()
      %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-09 00:00:00.000000Z], to: ~U[2020-08-09 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-11 00:00:00.000000Z], to: ~U[2020-08-11 23:59:59.999999Z]}
      ]}
  """
  @spec inverse(Slots.List.t()) :: Slots.List.t()
  @telemetria level: :info
  def inverse(slots)

  def inverse(%Slots.List{slots: [void()]}), do: %Slots.List{slots: []}
  def inverse(%Slots.List{slots: []}), do: %Slots.List{slots: [void()]}

  def inverse(%Slots.List{slots: list}),
    do: %Slots.List{slots: do_inverse(list, {[], nil})}

  defp do_inverse([], {slots, nil}), do: Enum.reverse(slots)
  defp do_inverse([], {slots, dt}), do: do_inverse([], {add_inversed_slot(dt, nil, slots), nil})

  defp do_inverse([%Slot{} = h | t], {[], nil}) when is_slot_open(h),
    do: do_inverse(t, {[], h.to})

  defp do_inverse([%Slot{} = h | t], {slots, dt}),
    do: do_inverse(t, {add_inversed_slot(dt, h.from, slots), h.to})

  defp add_inversed_slot(from, to, slots) do
    case Slot.shift(%Slot{from: from, to: to}, from: 1, to: -1) do
      void() -> slots
      slot -> [slot | slots]
    end
  end

  @spec split_while(Slots.List.t(), (Slot.t() -> boolean())) :: {Slots.List.t(), Slots.List.t()}
  def split_while(%Slots.List{slots: slots}, fun) do
    {h, t} = Enum.split_while(slots, fun)
    {%Slots.List{slots: h}, %Slots.List{slots: t}}
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
  def wrap(nil), do: %Slots.List{}

  def wrap(slots) when is_list(slots) do
    Enum.reduce(slots, %Slots.List{}, fn slot, acc -> Slots.List.add(acc, Slot.wrap(slot)) end)
  end

  def wrap(slot) when is_origin(slot), do: wrap([slot])

  defimpl Enumerable do
    @moduledoc false
    def reduce(_slots, {:halt, acc}, _fun), do: {:halted, acc}

    def reduce(%Slots.List{} = slots, {:suspend, acc}, fun),
      do: {:suspended, acc, &reduce(slots, &1, fun)}

    def reduce(%Slots.List{slots: []}, {:cont, acc}, _fun), do: {:done, acc}

    def reduce(%Slots.List{slots: [head | tail]}, {:cont, acc}, fun),
      do: reduce(%Slots.List{slots: tail}, fun.(head, acc), fun)

    def count(%Slots.List{slots: slots}), do: {:ok, length(slots)}
    def member?(%Slots.List{}, %Slot{}), do: {:error, __MODULE__}
    def slice(%Slots.List{}), do: {:error, __MODULE__}
  end

  defimpl Slots.Group do
    @moduledoc false
    def flatten(%Slots.List{slots: slots}), do: {:ok, slots}

    def next(%Slots.List{slots: slots}, origin), do: {:ok, do_next(slots, origin)}

    defp do_next([], _origin), do: nil
    defp do_next([%Slot{} = head | _], origin) when is_coming_before(origin, head), do: head
    defp do_next([%Slot{} = _ | tail], origin), do: do_next(tail, origin)

    def previous(%Slots.List{slots: [%Slot{} | _] = slots}, origin),
      do: {:ok, do_previous(slots, {origin, nil})}

    defp do_previous([], _origin), do: nil
    defp do_previous([%Slot{} = h | _], {origin, slot}) when is_coming_before(origin, h), do: slot
    defp do_previous([%Slot{} = head | tail], {origin, _}), do: do_previous(tail, {origin, head})

    # def add(%Slots.List{} = slots, slot, options),
    #   do: {:ok, Slots.List.add(slots, slot, options)}

    def merge(%Slots.List{} = slots, %Slots.List{} = other, options),
      do: {:ok, Slots.List.merge(slots, other, options)}

    def inverse(%Slots.List{} = slots),
      do: {:ok, Slots.List.inverse(slots)}
  end

  defimpl Collectable do
    @moduledoc false
    alias Tempus.Slots

    def into(original) do
      {
        original,
        fn
          slots, {:cont, value} -> Slots.List.add(slots, Slot.wrap(value))
          slots, :done -> slots
          _, :halt -> :ok
        end
      }
    end
  end

  defimpl Inspect do
    @moduledoc false
    import Inspect.Algebra

    def inspect(%Tempus.Slots.List{slots: slots}, opts) do
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

      concat(["𝕋", to_doc(inner_doc, opts)])
    end
  end
end
