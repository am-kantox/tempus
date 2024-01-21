defmodule Tempus.Slots.Stream do
  @moduledoc """
  The default `Stream` implementation of `Tempus.Slots` ordered collection.

  ### Examples

      iex> import Tempus.Slots.Stream, only: [slots: 0]
      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|),
      ...>   %Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}]
      ...> slots |> Enum.into(slots()) |> Enum.to_list()
      [%Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]},
       %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]
      iex> Enum.map(slots, & &1.from)
      [~U[2020-08-07 00:00:00.000000Z], ~U[2020-08-10 00:00:00.000000Z], ~U[2020-08-07 01:00:00Z]]
  """

  use Tempus.Telemetria

  alias Tempus.{Slot, Slots}

  import Tempus.Guards
  import Tempus.Slot, only: [void: 0]
  import Tempus.Slots.Normalizers

  defstruct slots: nil

  @type t :: Slots.implementation(Slots.Stream)

  @lookbehinds Application.compile_env(:tempus, :lookbehinds, 12)

  @doc false
  defmacro slots do
    case __CALLER__.context do
      nil ->
        quote generated: true, do: %Slots.Stream{slots: Stream.map([], & &1)}

      :match ->
        quote do
          %Slots.Stream{slots: var!(slots)}
          when is_struct(slots, Stream) or is_function(slots, 2)
        end

      :guard ->
        quote do
          %Slots.Stream{slots: slots}
          when is_struct(slots, Stream) or is_function(slots, 2)
        end
    end
  end

  @behaviour Slots.Behaviour

  @doc """
  Creates the new instance of `Tempus.Slots.Stream` struct. One usually does not need to call this function directly.
  """
  @impl Slots.Behaviour
  def new, do: slots()

  @doc """
  Adds another slot to the slots collection backed by stream.

  Joins slots intersecting with the new one, if any.

  ### Example

      iex> import Tempus.Slots.Stream, only: [slots: 0]
      iex> Tempus.Slots.Stream.add(slots(), Tempus.Slot.wrap(~D|2020-08-07|)) |> Enum.to_list()
      [%Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-07 23:59:59.999999Z]}]

      iex> %Tempus.Slots.Stream{}
      ...> |> Tempus.Slots.Stream.add(Tempus.Slot.wrap(~D|2020-08-07|))
      ...> |> Tempus.Slots.Stream.add(Tempus.Slot.wrap(~D|2020-08-10|))
      ...> |> Tempus.Slots.Stream.add(%Tempus.Slot{
      ...>       from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|})
      ...> |> Enum.to_list()
      [%Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 01:00:00Z]},
       %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]}]
  """
  @spec add(t(), Slot.t(), keyword()) :: t()
  @telemetria level: :debug
  def add(slots, slot, options \\ [])

  def add(%Slots.Stream{slots: nil}, slot, options),
    do: add(slots(), slot, options)

  def add(%Slots.Stream{slots: stream}, slot, options) do
    stream =
      options
      |> pop_jid()
      |> do_add(Slot.wrap(slot), stream)

    %Slots.Stream{slots: stream}
  end

  defp do_add(jid, slot, stream) do
    last_fun = fn
      nil -> {[], []}
      slot -> {[slot], []}
    end

    Stream.transform(stream, fn -> slot end, do_reducer(jid), last_fun, & &1)
  end

  defp do_reducer(nil) do
    fn
      slot, nil -> {[slot], nil}
      slot, s when is_coming_before(slot, s) -> {[slot], s}
      slot, s when is_coming_before(s, slot) -> {[s, slot], nil}
      slot, s -> {[], Slot.join(slot, s)}
    end
  end

  defp do_reducer(jid) do
    fn
      slot, nil ->
        {[slot], nil}

      slot, s ->
        cond do
          is_coming_before(slot, s) and not joint_in_delta?(slot, s, jid) ->
            {[slot], s}

          is_coming_before(s, slot) and not joint_in_delta?(s, slot, jid) ->
            {[s, slot], nil}

          true ->
            {[], Slot.join(slot, s)}
        end
    end
  end

  ###########################################################################

  @doc """
  Merges `other` into `this` slots instance. `other` might be `Enum` _or_ `Stream`.
  When `other` is a stream, it gets terminated immediately after the last element
  in `this`.

  ### Examples

      iex> import Tempus.Slots.Stream, only: [slots: 0]
      iex> slots = [
      ...>   Tempus.Slot.wrap(~D|2020-08-07|),
      ...>   Tempus.Slot.wrap(~D|2020-08-10|)
      ...> ] |> Enum.into(slots())
      iex> other = [
      ...>   %Tempus.Slot{from: ~U|2020-08-07 23:00:00Z|, to: ~U|2020-08-08 12:00:00Z|},
      ...>   %Tempus.Slot{from: ~U|2020-08-12 23:00:00Z|, to: ~U|2020-08-12 23:30:00Z|}
      ...> ] |> Enum.into(slots())
      iex> slots |> Tempus.Slots.Stream.merge(other) |> Enum.to_list()
      [%Tempus.Slot{from: ~U[2020-08-07 00:00:00.000000Z], to: ~U[2020-08-08 12:00:00Z]},
       %Tempus.Slot{from: ~U[2020-08-10 00:00:00.000000Z], to: ~U[2020-08-10 23:59:59.999999Z]},
       %Tempus.Slot{from: ~U[2020-08-12 23:00:00Z], to: ~U[2020-08-12 23:30:00Z]}]

  """
  @spec merge(t(), Slots.container(), keyword()) :: t()
  @telemetria level: :info
  def merge(slots, other, options \\ [])

  def merge(%Slots.Stream{slots: stream}, %Slots.List{slots: list}, options) do
    jid = pop_jid(options)

    split = fn slot, slots, jid ->
      head_splitter =
        if is_integer(jid) do
          &(is_coming_before(&1, slot) and not joint_in_delta?(&1, slot, jid))
        else
          &is_coming_before(&1, slot)
        end

      joint_splitter =
        if jid do
          &(not is_coming_before(slot, &1) or joint_in_delta?(slot, &1, jid))
        else
          &(not is_coming_before(slot, &1))
        end

      {to_emit, maybe_rest} = Enum.split_while(slots, head_splitter)
      {to_merge, rest} = Enum.split_while(maybe_rest, joint_splitter)
      {to_emit, [Tempus.Slot.join([slot | to_merge]) | rest]}
    end

    reducer = fn
      slot, [] ->
        {[slot], []}

      slot, [h | _] = list when is_coming_before(slot, h) and is_nil(jid) ->
        {[slot], list}

      slot, list ->
        split.(slot, list, jid)
    end

    stream = Stream.transform(stream, fn -> list end, reducer, fn acc -> {acc, []} end, & &1)
    %Slots.Stream{slots: stream}
  end

  def merge(%Slots.Stream{slots: stream}, %Slots.Stream{slots: other}, options) do
    jid = pop_jid(options)

    start_fun = fn -> {0, %Slots.List{}} end

    last_fun = fn {idx, acc} ->
      %Slots.Stream{slots: slots} =
        merge(
          %Slots.Stream{
            slots: [stream, other] |> Enum.map(&Stream.drop(&1, idx)) |> Stream.concat()
          },
          acc,
          options
        )

      {slots, []}
    end

    after_fun = & &1

    reducer = fn
      {e1, e2, idx}, {_, %Slots.List{slots: []}} when is_joint(e1, e2) ->
        {[], {idx, %Slots.List{slots: [Slot.join(e1, e2)]}}}

      {e1, e2, idx}, {_, %Slots.List{slots: []}} ->
        cond do
          is_integer(jid) && joint_in_delta?(e1, e2, jid) ->
            {[], {idx, %Slots.List{slots: [Slot.join(e1, e2)]}}}

          is_coming_before(e1, e2) ->
            {[e1], {idx, %Slots.List{slots: [e2]}}}

          is_coming_before(e2, e1) ->
            {[e2], {idx, %Slots.List{slots: [e1]}}}
        end

      {e1, e2, idx}, {_, acc} ->
        wrapper = if is_coming_before(e1.from, e2.from), do: e1, else: e2
        {to_emit, rest} = Enum.split_while(acc, &is_coming_before(&1, wrapper))
        {to_emit, {idx, %Slots.List{slots: rest} |> Slots.List.add(e1) |> Slots.List.add(e2)}}
    end

    merged =
      [stream, other, Stream.iterate(1, &(&1 + 1))]
      |> Stream.zip()
      |> Stream.transform(start_fun, reducer, last_fun, after_fun)

    %Slots.Stream{slots: merged}
  end

  def merge(%Slots.Stream{} = _list, _other, _options),
    do: raise(ArgumentError, message: "Merging different impls is not yet supported")

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
  @spec inverse(Slots.Stream.t()) :: Slots.Stream.t()
  @telemetria level: :info
  def inverse(slots, options \\ [])

  def inverse(%Slots.Stream{slots: stream}, _options) do
    start_fun = fn -> nil end

    last_fun = fn
      nil -> {[], []}
      dt -> {emit_inversed_slot(dt, nil), []}
    end

    after_fun = & &1

    reducer = fn
      void(), nil -> {:halt, :void}
      slot, nil when is_slot_open(slot) -> {[], slot.to}
      slot, nil -> {emit_inversed_slot(nil, slot.from), [slot.to]}
      slot, dt -> {emit_inversed_slot(Enum.min(dt, DateTime), slot.from), [slot.to]}
    end

    stream = Stream.transform(stream, start_fun, reducer, last_fun, after_fun)
    %Slots.Stream{slots: stream}
  end

  defp emit_inversed_slot(from, to) do
    case Slot.shift(%Slot{from: from, to: to}, from: 1, to: -1) do
      void() -> []
      slot -> [slot]
    end
  end

  @doc """
  Produces a stream of slots wrapped in `Tempus.Slots.Stream`, ensuring the order
    of elements emitted.

  ### Options

  - `join` (_default:_ `false`) — `nil | boolean() | non_neg_integer()` specifies how
    slots are to be joined
  - `return_as` (_default:_ `:stream`) — `:stream | :slots` if slots, the function returns
    the `t:Tempus.Slots.t/0` instance backed up by produced `t:Temput.Slots.Stream.t/0`

  By default, slots will not be joined. Pass `true` to join if they are 1μsec aside,
    or an integer specifying the number of μsecs to join.

  ### Examples

      iex> import Tempus.Sigils
      ...> Tempus.Slots.Stream.iterate(~D|2024-01-20|,
      ...>   &Tempus.Slot.shift(&1, by: rem(&1.from.day, 2) + 1, unit: :day),
      ...>   join: true) |> Enum.take(2)
      [~I(2024-01-20T00:00:00.000000Z → 2024-01-21T23:59:59.999999Z),
       ~I(2024-01-23T00:00:00.000000Z → 2024-01-23T23:59:59.999999Z)]
  """
  @spec iterate(Slot.origin(), (Slot.t() -> Slot.t())) :: Slots.Stream.t()
  @spec iterate(Slot.origin(), (Slot.t() -> Slot.t()), [
          {:join, nil | boolean() | pos_integer()} | {:return_as, :slots | :stream}
        ]) :: Slots.Stream.t() | Slots.t(Slots.Stream)
  def iterate(start_value, next_fun, options \\ []) do
    next_fun = &(&1 |> next_fun.() |> Slot.wrap())
    jid = pop_jid(options)

    stream =
      start_value
      |> Slot.wrap()
      |> Stream.unfold(fn
        %Slot{} = value ->
          if is_integer(jid),
            do: collect_joint(value, next_fun, jid),
            else: {value, next_fun.(value)}
      end)
      |> then(&%Slots.Stream{slots: &1})

    case Keyword.get(options, :return_as, :stream) do
      :stream -> stream
      :slots -> %Slots{slots: stream}
    end
  end

  defp collect_joint(%Slot{} = value, fun, join) do
    %Slot{} = next = fun.(value)

    if not is_slot_coming_before(value, next) do
      raise(
        ArgumentError,
        "Stream values must be increasing, got: [#{inspect(current: value, next: next)}]"
      )
    end

    if joint_in_delta?(value, next, join) do
      {curr, next} = collect_joint(next, fun, join)
      {Slot.join(value, curr), next}
    else
      {value, next}
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
      iex> slots =
      ...> [~D|2020-08-07|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|]
      ...> |> Enum.into(%Tempus.Slots.Stream{})
      iex> slots |> Tempus.Slots.Stream.split(~U|2020-08-09T12:00:00Z|) |> Tuple.to_list() |> Enum.map(&Enum.to_list/1)
      [
        [~I(2020-08-07T00:00:00.000000Z → 2020-08-07T23:59:59.999999Z), ~I(2020-08-08T00:00:00.000000Z → 2020-08-08T23:59:59.999999Z)],
        [~I(2020-08-10T00:00:00.000000Z → 2020-08-10T23:59:59.999999Z), ~I(2020-08-12T00:00:00.000000Z → 2020-08-12T23:59:59.999999Z)]
      ]
      iex> slots
      ...> |> Tempus.Slots.Stream.split(&is_coming_before(~U|2020-08-09T12:00:00Z|, &1))
      ...> |> Tuple.to_list()
      ...> |> Enum.map(&Enum.to_list/1)
      [
        [~I(2020-08-07T00:00:00.000000Z → 2020-08-07T23:59:59.999999Z), ~I(2020-08-08T00:00:00.000000Z → 2020-08-08T23:59:59.999999Z)],
        [~I(2020-08-10T00:00:00.000000Z → 2020-08-10T23:59:59.999999Z), ~I(2020-08-12T00:00:00.000000Z → 2020-08-12T23:59:59.999999Z)]
      ]
  """
  @spec split(t(), Slots.locator(), keyword()) :: {Enumerable.t(Slot.t()), Enumerable.t(Slot.t())}
  def split(%Slots.Stream{} = slots, pivot, options \\ []) when is_locator(pivot) do
    greedy? = Keyword.get(options, :greedy, true)
    adjustment = Keyword.get(options, :adjustment, 0)

    {head, joint, tail} = do_split_until(slots, pivot, adjustment)

    if greedy?,
      do: {Stream.concat(head, joint), Stream.concat(joint, tail)},
      else: {head, tail}
  end

  defmacrop match_chunk(count) do
    underscores = List.duplicate({:_, [], nil}, count + 1)

    quote do: [unquote_splicing(underscores), var!(slot)]
  end

  @compile {:inline, empty_stream: 0}
  defp empty_stream, do: Stream.map([], & &1)

  defp do_split_until(%Slots.Stream{} = stream, pivot, adjustment) when is_origin(pivot) do
    do_split_until(stream, pivot |> Slot.wrap() |> to_locator(), adjustment)
  end

  defp do_split_until(%Slots.Stream{slots: stream}, locator, 0) when is_function(locator, 1) do
    le_locator = &(locator.(&1) in [:lt, :eq, false])
    lt_locator = &(locator.(&1) in [:lt, false])

    tail = Stream.drop_while(stream, le_locator)
    head_and_joint = Stream.take_while(stream, le_locator)

    joint = Stream.drop_while(head_and_joint, lt_locator)
    head = Stream.take_while(head_and_joint, lt_locator)

    {head, joint, tail}
  end

  defp do_split_until(%Slots.Stream{} = stream, locator, adjustment)
       when is_function(locator, 1) and adjustment > 0 do
    {head, joint, tail} = do_split_until(stream, locator, 0)
    joint_size = joint |> Enum.to_list() |> length()

    if adjustment >= joint_size do
      head =
        head |> Stream.concat(joint) |> Stream.concat(Stream.take(tail, adjustment - joint_size))

      tail = Stream.drop(tail, adjustment - joint_size)
      {head, empty_stream(), tail}
    else
      head = head |> Stream.concat(Stream.take(joint, adjustment))
      tail = joint |> Stream.drop(adjustment) |> Stream.concat(tail)
      {head, empty_stream(), tail}
    end
  end

  defp do_split_until(slots, locator, adjustment),
    do: do_previous(slots, locator, -adjustment - 1)

  Enum.each(0..@lookbehinds, fn count ->
    defp do_previous(%Slots.Stream{slots: stream}, locator, unquote(count)) do
      stream = Stream.chunk_every(stream, unquote(count) + 2, 1, :discard)
      reducer = fn match_chunk(unquote(count)) -> locator.(slot) end

      head = Stream.take_while(stream, reducer)
      tail = Stream.drop_while(stream, reducer)

      [head, empty_stream(), tail]
      |> Enum.map(fn stream ->
        Stream.flat_map(stream, fn
          [result | _] -> [result]
          _ -> []
        end)
      end)
      |> List.to_tuple()
    end
  end)

  defp do_previous(_slots, _origin, count) when count > @lookbehinds do
    raise(
      ArgumentError,
      "Lookbehinds to more than #{@lookbehinds} slots are not supported, chain requests instead"
    )
  end

  defimpl Enumerable do
    @moduledoc false

    def reduce(%Slots.Stream{slots: function}, acc, fun) when is_function(function, 2),
      do: function.(acc, fun)

    def reduce(%Slots.Stream{slots: %Stream{} = stream}, acc, fun),
      do: Enumerable.reduce(stream, acc, fun)

    def count(%Slots.Stream{}), do: {:error, __MODULE__}
    def member?(%Slots.Stream{}, %Slot{}), do: {:error, __MODULE__}
    def slice(%Slots.Stream{}), do: {:error, __MODULE__}
  end

  defimpl Slots.Group do
    @moduledoc false

    def identity(%Slots.Stream{}), do: Slots.Stream.new()

    def flatten(%Slots.Stream{slots: stream}, options \\ []) do
      options
      |> Keyword.get(:until)
      |> case do
        nil -> stream
        count when is_integer(count) -> Stream.take(stream, count)
        origin when is_origin(origin) -> Stream.take_while(stream, &is_coming_before(&1, origin))
        fun when is_function(fun, 1) -> Stream.take_while(stream, &(not fun.(&1)))
      end
      |> Enum.to_list()
    end

    def add(%Slots.Stream{} = stream, slot, options), do: Slots.Stream.add(stream, slot, options)

    def split(%Slots.Stream{} = slots, pivot, options \\ []) when is_locator(pivot) do
      {head, tail} = Slots.Stream.split(slots, pivot, options)
      {:ok, %Slots.Stream{slots: head}, %Slots.Stream{slots: tail}}
    end

    def merge(%Slots.Stream{} = slots, other, options),
      do: {:ok, Slots.Stream.merge(slots, other, options)}

    def inverse(%Slots.Stream{} = slots, options \\ []),
      do: {:ok, Slots.Stream.inverse(slots, options)}
  end

  defimpl Collectable do
    @moduledoc false
    alias Tempus.Slots

    def into(original) do
      {
        original,
        fn
          slots, {:cont, value} -> Slots.Stream.add(slots, Slot.wrap(value))
          slots, :done -> slots
          _, :halt -> :ok
        end
      }
    end
  end

  defimpl Inspect do
    @moduledoc false
    import Inspect.Algebra

    def inspect(%Tempus.Slots.Stream{slots: slots}, opts) do
      concat(["𝕥ˢ<", to_doc(slots, opts), ">"])
    end
  end
end
