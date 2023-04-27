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
  import Tempus.Slots.Options

  defstruct slots: nil

  @type t :: Slots.t(Slots.Stream)

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

  @doc false
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
        if jid do
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

      slot, [h | _] = list when is_coming_before(slot, h) and jid == false ->
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
          jid && joint_in_delta?(e1, e2, jid) ->
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
      slot, dt -> {[emit_inversed_slot(dt, slot.from), slot.to]}
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

  By default, slots will be joined if they are 1μsec aside.
  """
  @spec iterate(Slot.origin(), (Slot.t() -> Slot.t()), keyword()) :: Slots.Stream.t()
  def iterate(start_value, next_fun, options \\ []) do
    next_fun = &(&1 |> next_fun.() |> Slot.wrap())
    jid = pop_jid(options)

    stream =
      Stream.unfold(Slot.wrap(start_value), fn
        %Slot{} = value ->
          if jid,
            do: collect_joint(value, next_fun, jid),
            else: {value, next_fun.(value)}
      end)

    %Slots.Stream{slots: stream}
  end

  defp collect_joint(%Slot{} = value, fun, join) do
    %Slot{} = next = fun.(value)

    if not is_coming_before(value, next),
      do: raise(ArgumentError, "Stream values must be increasing")

    if joint_in_delta?(value, next, join) do
      collect_joint(Slot.join(value, next), fun, join)
    else
      {value, next}
    end
  end

  defimpl Enumerable do
    @moduledoc false

    @compile :inline_list_funcs

    def reduce(%Slots.Stream{slots: function}, acc, fun) when is_function(function, 2),
      do: function.(acc, fun)

    def reduce(%Slots.Stream{slots: %Stream{} = stream}, acc, fun),
      do: Enumerable.reduce(stream, acc, fun)

    def count(%Slots.List{slots: slots}), do: {:ok, length(slots)}
    def member?(%Slots.List{}, %Slot{}), do: {:error, __MODULE__}
    def slice(%Slots.List{}), do: {:error, __MODULE__}
  end

  defimpl Slots.Group do
    @moduledoc false
    @lookbehinds Application.compile_env(:tempus, :lookbehinds, 12)

    defmacrop match_chunk(count) do
      underscores = List.duplicate({:_, [], nil}, count + 1)

      quote do: [unquote_splicing(underscores), var!(slot)]
    end

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

    def drop_until(slots, origin, options) do
      adjustment = Keyword.get(options, :adjustment, 0)
      do_drop_until(slots, origin, adjustment)
    end

    def do_drop_until(%Slots.Stream{slots: stream}, origin, adjustment) when adjustment >= 0 do
      origin = Slot.wrap(origin)

      tail =
        stream
        |> Stream.drop_while(&is_coming_before(&1, origin))
        |> Stream.drop(adjustment)

      {:ok, tail}
    end

    def do_drop_until(slots, origin, adjustment), do: do_previous(slots, origin, adjustment + 1)

    Enum.each(0..@lookbehinds, fn count ->
      defp do_previous(%Slots.Stream{slots: stream}, origin, unquote(count)) do
        origin = Slot.wrap(origin)

        tail =
          stream
          |> Stream.chunk_every(unquote(count) + 2, 1, :discard)
          |> Stream.drop_while(
            &match?(match_chunk(unquote(count)) when not is_coming_before(origin, slot), &1)
          )
          |> Stream.flat_map(fn
            [result | _] -> [result]
            _ -> []
          end)

        {:ok, tail}
      end
    end)

    defp do_previous(_slots, _origin, count) when count > @lookbehinds do
      raise(
        ArgumentError,
        "Lookbehinds to more than #{@lookbehinds} slots are not supported, chain requests instead"
      )
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
      concat(["𝕋ˢ<", to_doc(slots, opts), ">"])
    end
  end
end
