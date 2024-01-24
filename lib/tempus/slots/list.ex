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
  import Tempus.Slots.Normalizers

  @type t :: Slots.implementation(Slots.List, [Slot.t()])

  @lookbehinds Application.compile_env(:tempus, :lookbehinds, 12)

  defstruct slots: []

  @doc false
  defmacro slots, do: quote(do: %Tempus.Slots.List{})

  @behaviour Slots.Behaviour

  @doc """
  Creates the new instance of `Tempus.Slots.List` struct. One usually does not need to call this function directly.
  """
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
  @spec add(t(), Slot.origin(), keyword()) :: t()
  @telemetria level: :debug
  def add(%Slots.List{slots: slots}, slot, options \\ []) do
    slots =
      options
      |> pop_jid()
      |> do_add(Slot.wrap(slot), [], slots)

    %Slots.List{slots: slots}
  end

  @spec do_add(nil | non_neg_integer(), Slot.t(), [Slot.t()], [Slot.t()]) :: [Slot.t()]
  defp do_add(_, slot, head, []), do: Enum.reverse([slot | head])

  defp do_add(nil, slot, head, [th | tail]) when is_slot_coming_before(th, slot),
    do: do_add(nil, slot, [th | head], tail)

  defp do_add(nil, slot, head, [th | tail]) when is_slot_coming_before(slot, th),
    do: Enum.reverse(head) ++ [slot, th | tail]

  defp do_add(nil, slot, head, [th | tail]),
    do: do_add(nil, Slot.join(slot, th), head, tail)

  defp do_add(jid, slot, head, [th | tail]) do
    cond do
      is_slot_coming_before(slot, th) and not joint_in_delta?(slot, th, jid) ->
        Enum.reverse(head) ++ [slot, th | tail]

      is_slot_coming_before(th, slot) and not joint_in_delta?(th, slot, jid) ->
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
       when is_joint(hs, ho) and is_datetime_coming_before(hs.to, ho.to),
       do: do_merge_lists(jid, ts, [Slot.join(hs, ho) | to], result)

  defp do_merge_lists(jid, [hs | ts], [ho | to], result)
       when is_joint(hs, ho) and is_datetime_coming_before(ho.to, hs.to),
       do: do_merge_lists(jid, [Slot.join(hs, ho) | ts], to, result)

  defp do_merge_lists(jid, [hs | ts], [ho | to], result) when is_joint(hs, ho),
    do: do_merge_lists(jid, ts, to, [Slot.join(hs, ho) | result])

  # case: no check for joint in delta
  defp do_merge_lists(nil, [hs | ts], [ho | to], result) when is_slot_coming_before(hs, ho),
    do: do_merge_lists(nil, ts, [ho | to], [hs | result])

  defp do_merge_lists(nil, [hs | ts], [ho | to], result) when is_slot_coming_before(ho, hs),
    do: do_merge_lists(nil, [hs | ts], to, [ho | result])

  # case: check for joint in delta
  defp do_merge_lists(jid, [hs | ts], [ho | to], result) do
    cond do
      is_slot_coming_before(hs, ho) and not joint_in_delta?(hs, ho, jid) ->
        do_merge_lists(jid, ts, [ho | to], [hs | result])

      is_slot_coming_before(ho, hs) and not joint_in_delta?(ho, hs, jid) ->
        do_merge_lists(jid, [hs | ts], to, [ho | result])

      is_datetime_coming_before(hs.to, ho.to) ->
        do_merge_lists(jid, ts, [Slot.join(hs, ho) | to], result)

      is_datetime_coming_before(ho.to, hs.to) ->
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

      iex> [~D|2020-08-07|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|]
      ...> |> Enum.into(%Tempus.Slots.List{})
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

  @doc """
  Splits the slots by the pivot given as a `Slot.t` or as a function.

  To keep it consistent, the function actually does _split it until_,
  which is intuitive when the pivot is given and kinda counter-intuitive
  when the _locator_ function is given.

  See the examples below to grasp the reasoning behind this
  architectural decision.

  ### Examples

      iex> import Tempus.Guards
      ...> import Tempus.Sigils
      ...> slots =
      ...> [~D|2020-08-07|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|]
      ...> |> Enum.into(%Tempus.Slots.List{})
      iex> slots |> Tempus.Slots.List.split(~U|2020-08-09T12:00:00Z|)
      {
        [~I(2020-08-07T00:00:00.000000Z → 2020-08-07T23:59:59.999999Z), ~I(2020-08-08T00:00:00.000000Z → 2020-08-08T23:59:59.999999Z)],
        [~I(2020-08-10T00:00:00.000000Z → 2020-08-10T23:59:59.999999Z), ~I(2020-08-12T00:00:00.000000Z → 2020-08-12T23:59:59.999999Z)]
      }
      iex> slots |> Tempus.Slots.List.split(&is_slot_coming_before(Tempus.Slot.wrap(~U|2020-08-09T12:00:00Z|), &1))
      {
        [~I(2020-08-07T00:00:00.000000Z → 2020-08-07T23:59:59.999999Z), ~I(2020-08-08T00:00:00.000000Z → 2020-08-08T23:59:59.999999Z)],
        [~I(2020-08-10T00:00:00.000000Z → 2020-08-10T23:59:59.999999Z), ~I(2020-08-12T00:00:00.000000Z → 2020-08-12T23:59:59.999999Z)]
      }
  """
  @spec split(t(), Slots.locator(), keyword()) :: {[Slot.t()], [Slot.t()]}
  def split(slots, pivot, options \\ []) when is_locator(pivot) do
    greedy? = Keyword.get(options, :greedy, true)
    adjustment = Keyword.get(options, :adjustment, 0)

    {tail, reversed_head} = do_split_until(slots, pivot, adjustment)
    head = Enum.reverse(reversed_head)

    case {greedy?, pivot} do
      {true, origin} when is_origin(origin) ->
        {head ++ Enum.take_while(tail, &is_joint(&1, Slot.wrap(origin))), tail}

      {false, origin} when is_origin(origin) ->
        {head, Enum.drop_while(tail, &is_joint(&1, Slot.wrap(origin)))}

      _ ->
        {head, tail}
    end
  end

  @spec do_split_until(t(), Slots.locator(), non_neg_integer()) :: {[Slot.t()], [Slot.t()]}
  defp do_split_until(%Slots.List{slots: slots}, origin, adjustment)
       when is_origin(origin) and adjustment >= 0,
       do: do_next(slots, Slot.wrap(origin), adjustment, [])

  defp do_split_until(%Slots.List{slots: slots}, origin, adjustment) when is_origin(origin),
    do: do_previous(slots, Slot.wrap(origin), -adjustment, [])

  defp do_split_until(%Slots.List{slots: slots}, locator, adjustment)
       when is_function(locator, 1) and adjustment >= 0,
       do: do_next(slots, locator, adjustment, [])

  defp do_split_until(%Slots.List{slots: slots}, locator, adjustment)
       when is_function(locator, 1),
       do: do_previous(slots, locator, -adjustment, [])

  defp do_next([], _pivot, _count, acc), do: {[], acc}

  defp do_next([%Slot{} = head | _] = list, %Slot{} = origin, count, acc)
       when not is_slot_coming_before(head, origin),
       do: do_skip(list, count, acc)

  defp do_next([%Slot{} = head | tail], %Slot{} = origin, count, acc),
    do: do_next(tail, origin, count, [head | acc])

  defp do_next([%Slot{} = head | tail] = list, locator, count, acc) when is_function(locator, 1),
    do:
      if(locator.(head),
        do: do_skip(list, count, acc),
        else: do_next(tail, locator, count, [head | acc])
      )

  defp do_skip([], _count, acc), do: {[], acc}
  defp do_skip([_ | _] = result, count, acc) when count <= 0, do: {result, acc}
  defp do_skip([head | tail], count, acc), do: do_skip(tail, count - 1, [head | acc])

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

  Enum.each(0..@lookbehinds, fn count ->
    defp do_previous(match_n_slots(unquote(count)), %Slot{} = origin, unquote(count), acc)
         when not is_slot_coming_before(origin, slot_before) and
                is_slot_coming_before(origin, slot_after),
         do: {list, acc}

    defp do_previous(match_n_slots(unquote(count)), %Slot{} = origin, unquote(count), acc),
      do: do_previous(tl(list), origin, unquote(count), [head | acc])

    defp do_previous(match_n_slots(unquote(count)), locator, unquote(count), acc)
         when is_function(locator, 1) do
      if not locator.(slot_before) and locator.(slot_after),
        do: {list, acc},
        else: do_previous(tl(list), locator, unquote(count), [head | acc])
    end
  end)

  defp do_previous(_slots, _origin, count, _acc) when count > @lookbehinds do
    raise(
      ArgumentError,
      "Lookbehinds to more than #{@lookbehinds} slots are not supported (#{count} requested), chain requests instead"
    )
  end

  defp do_previous(slots, _origin, _count, acc), do: {Enum.reverse(acc) ++ slots, []}

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

    def identity(%Slots.List{slots: _}), do: %Slots.List{slots: []}

    def flatten(%Slots.List{slots: slots}, _options \\ []), do: slots

    def add(%Slots.List{} = slots, slot, options \\ []) when is_origin(slot),
      do: Slots.List.add(slots, slot, options)

    def split(slots, pivot, options \\ []) when is_locator(pivot) do
      {head, tail} = Slots.List.split(slots, pivot, options)
      {:ok, %Slots.List{slots: head}, %Slots.List{slots: tail}}
    end

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
          i when is_integer(i) and i >= 0 and length(slots) > 3 ->
            [hd(slots), "… ‹#{length(slots) - 2 - i} more› …" | Enum.slice(slots, -i - 1, i + 1)]

          _ ->
            slots
        end

      concat(["𝕥ˡ<", to_doc(inner_doc, opts), ">"])
    end
  end
end
