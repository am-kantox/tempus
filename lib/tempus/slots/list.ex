defmodule Tempus.Slots.List do
  @moduledoc """
  The default `List` implementation of `Tempus.Slots` ordered collection.

  ### Examples

      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}]
      ...> Enum.into(slots, %Tempus.Slots.List{})
      %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]}
      iex> Enum.map(slots, & &1.from)
      [~U[2020-08-07 00:00:00.000000Z], ~U[2020-08-10 00:00:00.000000Z], ~U[2020-08-07 01:00:00Z]]
  """

  use Tempus.Telemetria

  alias Tempus.{Slot, Slots}

  import Tempus.Guards
  import Tempus.Slot, only: [void: 0]
  import Tempus.Slots.Options

  @type t :: Slots.t(Slots.List)

  defstruct slots: []

  @doc false
  defmacro slots, do: quote(do: %Tempus.Slots.List{})

  @behaviour Slots.Behaviour

  @doc false
  @impl Slots.Behaviour
  def new, do: slots()

  @doc """
  Adds another slot to the slots collection implemented as list.

  Joins slots intersecting with the new one, if any.

  ### Example

      iex> Tempus.Slots.List.add(%Tempus.Slots.List{}, Tempus.Slot.wrap(~D|2020-08-07|))
      %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}]}

      iex> %Tempus.Slots.List{}
      ...> |> Tempus.Slots.List.add(Tempus.Slot.wrap(~D|2020-08-07|))
      ...> |> Tempus.Slots.List.add(Tempus.Slot.wrap(~D|2020-08-10|))
      ...> |> Tempus.Slots.List.add(%Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|})
      %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]}
  """
  @spec add(t(), Slot.t(), keyword()) :: t()
  @telemetria level: :debug
  def add(%Slots.List{slots: slots}, slot, options \\ []) do
    slots =
      options
      |> pop_jid()
      |> do_add(Slot.wrap(slot), [], slots)

    %Slots.List{slots: slots}
  end

  defp do_add(_, slot, head, []), do: Enum.reverse([slot | head])

  defp do_add(nil, slot, head, [th | tail]) when is_coming_before(th, slot),
    do: do_add(nil, slot, [th | head], tail)

  defp do_add(nil, slot, head, [th | tail]) when is_coming_before(slot, th),
    do: Enum.reverse(head) ++ [slot, th | tail]

  defp do_add(nil, slot, head, [th | tail]),
    do: do_add(nil, Slot.join(slot, th), head, tail)

  defp do_add(jid, slot, head, [th | tail]) do
    cond do
      is_coming_before(slot, th) and not joint_in_delta?(slot, th, jid) ->
        Enum.reverse(head) ++ [slot, th | tail]

      is_coming_before(th, slot) and not joint_in_delta?(th, slot, jid) ->
        do_add(jid, slot, [th | head], tail)

      true ->
        do_add(jid, Slot.join(slot, th), head, tail)
    end
  end

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
      iex> Tempus.Slots.merge([%Tempus.Slots{slots: slots}, other]).slots
      %Tempus.Slots.List{slots: [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]}]}
      iex> %Tempus.Slots{slots: slots} |> Tempus.Slots.merge(Tempus.Slots.wrap(other, Tempus.Slots.Stream)) |> Enum.to_list()
      [
        %Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]},
        %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]},
        %Tempus.Slot{from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]}]
  """
  @spec merge(t(), t() | Tempus.Slots.Stream.t(), keyword()) :: t()
  @telemetria level: :debug
  def merge(slots, other, options \\ [])

  def merge(%Slots.List{slots: slots}, %Slots.List{slots: other}, options) do
    slots =
      options
      |> pop_jid()
      |> do_merge_lists(slots, other, [])

    %Slots.List{slots: slots}
  end

  def merge(%Slots.List{} = list, %Slots.Stream{} = stream, options),
    do: Slots.Stream.merge(stream, list, options)

  def merge(%Slots.List{} = _list, _other, _options),
    do: raise(ArgumentError, message: "Merging different impls is not yet supported")

  defp do_merge_lists(jid, [], other, result), do: do_maybe_join(jid, result, other)
  defp do_merge_lists(jid, slots, [], result), do: do_maybe_join(jid, result, slots)

  # case: slots are joint
  defp do_merge_lists(jid, [hs | ts], [ho | to], result)
       when is_joint(hs, ho) and is_coming_before(hs.to, ho.to),
       do: do_merge_lists(jid, ts, [Slot.join(hs, ho) | to], result)

  defp do_merge_lists(jid, [hs | ts], [ho | to], result)
       when is_joint(hs, ho) and is_coming_before(ho.to, hs.to),
       do: do_merge_lists(jid, [Slot.join(hs, ho) | ts], to, result)

  defp do_merge_lists(jid, [hs | ts], [ho | to], result) when is_joint(hs, ho),
    do: do_merge_lists(jid, ts, to, [Slot.join(hs, ho) | result])

  # case: no check for joint in delta
  defp do_merge_lists(nil, [hs | ts], [ho | to], result) when is_coming_before(hs, ho),
    do: do_merge_lists(nil, ts, [ho | to], [hs | result])

  defp do_merge_lists(nil, [hs | ts], [ho | to], result) when is_coming_before(ho, hs),
    do: do_merge_lists(nil, [hs | ts], to, [ho | result])

  # case: check for joint in delta
  defp do_merge_lists(jid, [hs | ts], [ho | to], result) do
    cond do
      is_coming_before(hs, ho) and not joint_in_delta?(hs, ho, jid) ->
        do_merge_lists(jid, ts, [ho | to], [hs | result])

      is_coming_before(ho, hs) and not joint_in_delta?(ho, hs, jid) ->
        do_merge_lists(jid, [hs | ts], to, [ho | result])

      is_coming_before(hs.to, ho.to) ->
        do_merge_lists(jid, ts, [Slot.join(hs, ho) | to], result)

      is_coming_before(ho.to, hs.to) ->
        do_merge_lists(jid, [Slot.join(hs, ho) | ts], to, result)

      true ->
        # this is unreachable, but `cond/1` requires `true`
        do_merge_lists(jid, ts, to, [Slot.join(hs, ho) | result])
    end
  end

  defp do_maybe_join(_, [], tail), do: tail
  defp do_maybe_join(_, head, []), do: Enum.reverse(head)
  defp do_maybe_join(nil, head, tail), do: Enum.reverse(head) ++ tail

  defp do_maybe_join(jid, [h | head], [t | tail]) do
    if joint_in_delta?(h, t, jid),
      do: do_maybe_join(jid, [Slot.join(h, t) | head], tail),
      else: do_maybe_join(nil, head, tail)
  end

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
  @spec inverse(Slots.List.t(), keyword()) :: Slots.List.t()
  @telemetria level: :debug
  def inverse(slots, options \\ [])

  def inverse(%Slots.List{slots: [void()]}, _options), do: %Slots.List{slots: []}
  def inverse(%Slots.List{slots: []}, _options), do: %Slots.List{slots: [void()]}

  def inverse(%Slots.List{slots: list}, _options),
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

    @lookbehinds Application.compile_env(:tempus, :lookbehinds, 12)

    defmacrop match_n_slots(0) do
      quote generated: true,
            do: [var!(slot_before) = var!(head), var!(slot_after) | _] = var!(list)
    end

    defmacrop match_n_slots(1) do
      quote generated: true,
            do: [var!(head), var!(slot_before), var!(slot_after) | _] = var!(list)
    end

    defmacrop match_n_slots(count) when count >= 2 do
      underscores = List.duplicate({:_, [], nil}, count - 1)

      quote generated: true do
        [var!(head), unquote_splicing(underscores), var!(slot_before), var!(slot_after) | _] =
          var!(list)
      end
    end

    def identity(%Slots.List{slots: _}), do: %Slots.List{slots: []}
    def flatten(%Slots.List{slots: slots}, _options \\ []), do: slots
    def add(%Slots.List{} = slots, slot, options \\ []), do: Slots.List.add(slots, slot, options)

    def drop_until(slots, origin, options \\ []) do
      adjustment = Keyword.get(options, :adjustment, 0)
      slots = do_drop_until(slots, origin, adjustment)
      {:ok, %Slots.List{slots: slots}}
    end

    defp do_drop_until(%Slots.List{slots: slots}, origin, adjustment) when adjustment >= 0,
      do: do_next(slots, Slot.wrap(origin), adjustment)

    defp do_drop_until(%Slots.List{slots: slots}, origin, adjustment),
      do: do_previous(slots, Slot.wrap(origin), adjustment + 1)

    defp do_next([], _origin, _count), do: []

    defp do_next([%Slot{} = head | _] = list, origin, count)
         when not is_coming_before(head, origin),
         do: do_skip(list, count)

    defp do_next([%Slot{} = _ | tail], origin, count), do: do_next(tail, origin, count)

    defp do_skip([], _count), do: []
    defp do_skip([_ | _] = result, count) when count <= 0, do: result
    defp do_skip([_ | t], count), do: do_skip(t, count - 1)

    Enum.each(0..@lookbehinds, fn count ->
      defp do_previous(match_n_slots(unquote(count)), origin, unquote(count))
           when not is_coming_before(origin, slot_before) and
                  is_coming_before(origin, slot_after),
           do: list

      defp do_previous(match_n_slots(unquote(count)), origin, unquote(count)),
        do: do_previous(tl(list), origin, unquote(count))
    end)

    defp do_previous(_slots, _origin, count) when count > @lookbehinds do
      raise(
        ArgumentError,
        "Lookbehinds to more than #{@lookbehinds} slots are not supported, chain requests instead"
      )
    end

    defp do_previous(_, _origin, _count), do: nil

    def merge(%Slots.List{} = slots, %_{} = other, options),
      do: {:ok, Slots.List.merge(slots, other, options)}

    def inverse(%Slots.List{} = slots, options),
      do: {:ok, Slots.List.inverse(slots, options)}
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
        |> Keyword.get(:truncate, true)
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

      concat(["𝕋ˡ<", to_doc(inner_doc, opts), ">"])
    end
  end
end
