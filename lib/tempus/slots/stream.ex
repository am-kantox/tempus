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

  defstruct slots: nil

  @type t :: %{
          __struct__: Tempus.Slots.Stream,
          slots: Enumerable.t(Slot.t())
        }

  @doc false
  defmacro slots do
    quote do: %Tempus.Slots.Stream{slots: Stream.map([], & &1)}
  end

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

  def add(%Slots.Stream{slots: nil}, %Slot{} = slot, options),
    do: add(slots(), slot, options)

  def add(%Slots.Stream{slots: stream}, %Slot{} = slot, _options) do
    last_fun = fn
      nil -> {[], []}
      slot -> {[slot], []}
    end

    reducer = fn
      slot, nil -> {[slot], nil}
      slot, s when is_coming_before(slot, s) -> {[slot], s}
      slot, s when is_coming_before(s, slot) -> {[s, slot], nil}
      slot, s -> {[Slot.join(slot, s)], nil}
    end

    stream = Stream.transform(stream, fn -> slot end, reducer, last_fun, & &1)
    %Slots.Stream{slots: stream}
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

  def merge(%Slots.Stream{slots: stream}, %Slots.List{slots: list}, _options) do
    reducer = fn
      slot, [] ->
        {[slot], []}

      slot, [h | _] = list when is_coming_before(slot, h) ->
        {[slot], list}

      slot, list ->
        {to_emit, maybe_rest} = Enum.split_while(list, &is_coming_before(&1, slot))
        {to_merge, rest} = Enum.split_while(maybe_rest, &(not is_coming_before(slot, &1)))
        {to_emit ++ [Tempus.Slot.join([slot | to_merge])], rest}
    end

    stream = Stream.transform(stream, fn -> list end, reducer, fn acc -> {acc, []} end, & &1)
    %Slots.Stream{slots: stream}
  end

  def merge(%Slots.Stream{slots: stream}, %Slots.Stream{slots: other}, _options) do
    start_fun = fn -> {0, %Slots.List{}} end

    last_fun = fn {idx, acc} ->
      %Slots.Stream{slots: slots} =
        merge(
          %Slots.Stream{
            slots: [stream, other] |> Enum.map(&Stream.drop(&1, idx)) |> Stream.concat()
          },
          acc
        )

      {slots, []}
    end

    after_fun = & &1

    reducer = fn
      {e1, e2, idx}, {_, %Slots.List{slots: []}} when is_coming_before(e1, e2) ->
        {[e1], {idx, %Slots.List{slots: [e2]}}}

      {e1, e2, idx}, {_, %Slots.List{slots: []}} when is_coming_before(e2, e1) ->
        {[e2], {idx, %Slots.List{slots: [e1]}}}

      {e1, e2, idx}, {_, %Slots.List{slots: []}} ->
        {[], {idx, %Slots.List{slots: [Slot.join(e1, e2)]}}}

      {e1, e2, idx}, {_, acc} ->
        wrapper = if is_coming_before(e1.from, e2.from), do: e1, else: e2

        {%Slots.List{slots: to_emit}, rest} =
          Slots.List.split_while(acc, &is_coming_before(&1, wrapper))

        {to_emit, {idx, rest |> Slots.List.add(e1) |> Slots.List.add(e2)}}
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
  def inverse(slots)

  def inverse(%Slots.Stream{slots: stream}) do
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

  @spec wrap(Slot.t() | [Slot.t()]) :: Slots.Stream.t()
  @doc since: "0.3.0"
  @doc """
  Wraps the argument into a slots instance. For `nil` it’d be an empty slots.
  For everything else it’d call `Slot.wrap/1` on an argument and add it to empty slots.

  ## Examples

      iex> Tempus.Slots.Stream.wrap(~D|2020-08-06|) |> Enum.to_list()
      [%Tempus.Slot{from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]}]
  """
  def wrap(nil), do: Slots.Stream.new()

  def wrap(slots) when is_list(slots) do
    list =
      Enum.reduce(slots, %Slots.List{}, fn slot, acc -> Slots.List.add(acc, Slot.wrap(slot)) end)

    %Slots.Stream{slots: Stream.map(list, & &1)}
  end

  def wrap(slot) when is_origin(slot), do: wrap([slot])

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
    def flatten(%Slots.Stream{slots: stream}), do: {:ok, Enum.to_list(stream)}

    def next(%Slots.Stream{slots: stream}, origin) do
      slot =
        stream
        |> Stream.drop_while(&(not is_coming_before(origin, &1)))
        |> Enum.take(1)
        |> List.first()

      {:ok, slot}
    end

    def previous(%Slots.Stream{slots: stream}, origin) do
      slot =
        stream
        |> Stream.chunk_every(2, 1)
        |> Stream.drop_while(&match?({_, e} when is_coming_before(e, origin), &1))
        |> Enum.take(1)
        |> case do
          [] -> nil
          [{found, _}] -> found
        end

      {:ok, slot}
    end

    def merge(%Slots.Stream{} = slots, %Slots.Stream{} = other, options),
      do: {:ok, Slots.Stream.merge(slots, other, options)}

    def inverse(%Slots.Stream{} = slots),
      do: {:ok, Slots.Stream.inverse(slots)}
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
      concat(["𝕋[", to_doc(slots, opts), "]"])
    end
  end
end
