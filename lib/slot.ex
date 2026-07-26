defmodule Tempus.Slot do
  @moduledoc """
  Declares a time slot interval and provides functions for interval arithmetic,
  coverage checks, boundary comparisons, and transformations.

  Slots in `Tempus` are modeled as mathematical intervals with configurable boundary openness:
  - **Half-Open Intervals**: Standard slots default to $[from, to)$ (`from_open: false`, `to_open: true`).
    For example, a date `~D[2023-04-12]` wraps into `%Slot{from: ~U[2023-04-12 00:00:00Z], to: ~U[2023-04-13 00:00:00Z], from_open: false, to_open: true}`.
  - **Infinite / Unbound Limits**: A `nil` boundary represents infinity ($-\infty$ or $+\infty$) and is always treated as **open** (`from_open: true` when `from: nil`, `to_open: true` when `to: nil`).
  - **Void / Identity Slot**: Represented by `%Slot{from: nil, to: nil, from_open: true, to_open: true}` (`Slot.id/0`).

  ### Key Functions
  - `Slot.wrap/1`: Converts a `Date`, `DateTime`, or `Time` into a `%Tempus.Slot{}`.
  - `Slot.join/2`: Merges two overlapping or contiguous slots into a single maximal slot.
  - `Slot.intersect/2`: Calculates the overlapping intersection of slots.
  - `Slot.xor/2`: Returns the symmetric difference between two slots.
  - `Slot.gap/1`: Finds the uncovered gap between slots.
  - `Slot.cover?/2`: Checks if a `DateTime` or `Date` falls within the slot interval.
  - `Slot.disjoint?/2` & `Slot.neighbour?/2`: Checks if slots are non-overlapping or adjacent/touching.
  """
  alias __MODULE__

  import Tempus.Guards

  @typedoc "A timeslot to be used in `Tempus`"
  @type t :: %__MODULE__{
          from: nil | DateTime.t(),
          to: nil | DateTime.t(),
          from_open: boolean(),
          to_open: boolean()
        }

  @typedoc "The origin used in comparisons and calculations"
  @type origin :: Slot.t() | Date.t() | DateTime.t() | nil

  defstruct [:from, :to, from_open: false, to_open: true]

  @spec new([{:from, origin()} | {:to, origin()}]) :: {:ok, t()} | {:error, any()}
  @doc """
  Creates new slot using `arg[:from]` as a starting origin and `arg[:to]` and an ending origin.

  ## Examples

      iex> Tempus.Slot.new(from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|)
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|, from_open: false, to_open: false}}
      iex> Tempus.Slot.new(%{from: ~D|2015-09-30|, to: ~U|2015-10-01T12:00:00Z|})
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00.000000Z|, to: ~U|2015-10-01 12:00:00Z|, from_open: false, to_open: false}}
  """
  def new(from_to), do: new(from_to[:from], from_to[:to])

  @spec new(from :: origin(), to :: origin()) :: {:ok, t()} | {:error, any()}
  @doc """
  Creates new slot using `from` as a starting origin and `to` and an ending origin.
  See `new/1` for more readable implementation.

  ## Examples

      iex> import Tempus.Sigils
      iex> Tempus.Slot.new(~U|2015-09-30 00:00:00Z|, ~U|2015-10-01 01:00:00Z|)
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|, from_open: false, to_open: false}}
      iex> Tempus.Slot.new(~D|2015-09-30|, ~U|2015-10-01T12:00:00Z|)
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00.000000Z|, to: ~U|2015-10-01 12:00:00Z|, from_open: false, to_open: false}}
      iex> Tempus.Slot.new(nil, nil)
      {:ok, Tempus.Slot.id()}
      iex> Tempus.Slot.new(~D|2015-09-30|, nil)
      {:ok, %Tempus.Slot{from: ~U[2015-09-30 00:00:00.000000Z], to: nil, from_open: false, to_open: true}}
      iex> Tempus.Slot.new(nil, ~D|2015-09-30|)
      {:ok, %Tempus.Slot{from: nil, to: ~U[2015-10-01 00:00:00.000000Z], from_open: true, to_open: true}}
      iex> Tempus.Slot.new(:ok, :ok)
      {:error, :invalid_input}
  """
  def new(from, to) when not is_origin(from) when not is_origin(to), do: {:error, :invalid_input}
  def new(nil, nil), do: {:ok, %Tempus.Slot{from: nil, to: nil, from_open: true, to_open: true}}

  def new(from, nil) do
    w = wrap(from)
    {:ok, %Tempus.Slot{from: w.from, to: nil, from_open: w.from_open, to_open: true}}
  end

  def new(nil, to) do
    w = wrap(to)
    {:ok, %Tempus.Slot{from: nil, to: w.to, from_open: true, to_open: w.to_open}}
  end

  def new(from, to), do: {:ok, [from, to] |> Enum.map(&wrap/1) |> join()}

  @spec new!(from :: origin(), to :: origin()) :: t() | no_return

  @doc """
  Creates new slot using `from` as a starting origin and `to` and an ending origin.
  Unlike `new/1`, this function raises on malformed input.

  ## Examples

      iex> Tempus.Slot.new!(~U|2015-09-30 00:00:00Z|, ~U|2015-10-01 01:00:00Z|)
      %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|, from_open: false, to_open: false}
      iex> Tempus.Slot.new!(~D|2015-09-30|, ~U|2015-10-01T12:00:00Z|)
      %Tempus.Slot{from: ~U|2015-09-30 00:00:00.000000Z|, to: ~U|2015-10-01 12:00:00Z|, from_open: false, to_open: false}
      iex> Tempus.Slot.new!(:ok, :ok)
      ** (ArgumentError) malformed from/to argument, expected `origin`
  """
  def new!(from, to) do
    case new(from, to) do
      {:ok, slot} ->
        slot

      {:error, :invalid_input} ->
        raise ArgumentError, message: "malformed from/to argument, expected `origin`"
    end
  end

  @doc """
  Helper macro to pattern-match void slots.
  """
  defmacro void do
    quote do
      %Slot{from: nil, to: nil, from_open: true, to_open: true}
    end
  end

  @doc "Identity element, void slot `~I(nil → nil)`"
  @spec id :: %{__struct__: Slot, from: nil, to: nil, from_open: true, to_open: true}
  def id, do: %Slot{from: nil, to: nil, from_open: true, to_open: true}

  @spec valid?(slot :: Slot.t()) :: boolean()
  @doc """
  Checks whether the `Slot` is valid (to > from, or to == from if closed point) or not.

  ## Examples

      iex> slot = %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|}
      iex> Tempus.Slot.valid?(slot)
      true
      iex> Tempus.Slot.valid?(%Tempus.Slot{from: slot.to, to: slot.from})
      false
      iex> slot = %Tempus.Slot{from: nil, to: ~U|2015-10-01 01:00:00Z|}
      ...> Tempus.Slot.valid?(slot)
      true
      iex> slot = %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: nil}
      ...> Tempus.Slot.valid?(slot)
      true
      iex> Tempus.Slot.valid?(:ok)
      false
  """
  def valid?(%Slot{from: nil, to: nil}), do: true
  def valid?(%Slot{from: nil, to: %DateTime{}}), do: true
  def valid?(%Slot{from: %DateTime{}, to: nil}), do: true

  def valid?(%Slot{from: %DateTime{} = from, to: %DateTime{} = to, from_open: fo, to_open: to_op}) do
    case DateTime.compare(from, to) do
      :lt -> true
      :eq -> not fo and not to_op
      :gt -> false
    end
  end

  def valid?(_), do: false

  @doc """
  Splits the slot given as a first argument to two on borders given as a second slot.

  ## Examples

      iex> outer = Tempus.Slot.wrap(~D[2023-04-12])
      ...> {:ok, inner} = Tempus.Slot.new(~U[2023-04-12 12:00:00Z], ~U[2023-04-12 13:00:00Z])
      iex> Tempus.Slot.xor(outer, inner)
      [%Tempus.Slot{from: ~U[2023-04-12 00:00:00.000000Z], to: ~U[2023-04-12 12:00:00Z], from_open: false, to_open: true},
       %Tempus.Slot{from: ~U[2023-04-12 13:00:00Z], to: ~U[2023-04-13 00:00:00.000000Z], from_open: true, to_open: true}]
      iex> Tempus.Slot.xor(outer, inner) == Tempus.Slot.xor(inner, outer)
      false
      iex> {:ok, past} = Tempus.Slot.new(~U[2020-04-12 12:00:00Z], ~U[2020-04-12 13:00:00Z])
      ...> Tempus.Slot.xor(past, inner)
      [past, inner]
      ...> Tempus.Slot.xor(inner, past)
      [past, inner]
      iex> {:ok, border} = Tempus.Slot.new(~U[2023-04-12 11:00:00Z], ~U[2023-04-12 12:00:00Z])
      ...> Tempus.Slot.xor(border, inner)
      [Tempus.Slot.new!(~U[2023-04-12 11:00:00Z], ~U[2023-04-12 13:00:00Z])]
  """
  @spec xor(outer :: Slot.t(), inner :: Slot.t()) :: [Slot.t()]
  def xor(outer, inner) when is_slot_coming_before(outer, inner), do: [outer, inner]
  def xor(outer, inner) when is_slot_coming_before(inner, outer), do: [inner, outer]

  def xor(outer, inner) when is_slot_border(inner.from, outer) or is_slot_border(inner.to, outer),
    do: [Slot.join(inner, outer)]

  def xor(outer, inner) do
    s1 = %Slot{
      from: outer.from,
      to: inner.from,
      from_open: outer.from_open,
      to_open: not inner.from_open
    }

    s2 = %Slot{
      from: inner.to,
      to: outer.to,
      from_open: not inner.to_open,
      to_open: outer.to_open
    }

    Enum.filter([s1, s2], &valid?/1)
  end

  @spec cover?(slot :: Slot.t(), dt :: origin(), strict? :: boolean()) ::
          boolean()
  @doc """
  Checks whether the `Slot` covers the date/datetime passed as a second argument.

  ## Examples

      iex> dt_between = ~U|2015-09-30 01:00:00Z|
      ...> dt_from = ~U|2015-09-30 00:00:00Z|
      ...> dt_to = ~U|2015-10-01 01:00:00Z|
      ...> d_from = Date.from_iso8601!("2015-09-30")
      ...> d_to = Date.from_iso8601!("2015-10-01")
      iex> slot = %Tempus.Slot{from: dt_from, to: dt_to}
      iex> Tempus.Slot.cover?(slot, dt_between)
      true
      iex> Tempus.Slot.cover?(slot, dt_to)
      false
      iex> Tempus.Slot.cover?(slot, dt_from)
      true
      iex> Tempus.Slot.cover?(slot, d_from)
      true
      iex> Tempus.Slot.cover?(slot, ~U|2000-01-01 00:00:00Z|)
      false
      iex> Tempus.Slot.cover?(slot, d_to)
      false
  """
  def cover?(slot, dt, strict? \\ false)

  def cover?(%Slot{} = slot, %DateTime{} = dt, _) when not is_datetime_covered(dt, slot),
    do: false

  def cover?(%Slot{} = slot, %DateTime{} = dt, true) when is_slot_border(dt, slot), do: false
  def cover?(%Slot{}, %DateTime{}, _), do: true
  def cover?(%Slot{} = slot, %Slot{} = dt, _) when not is_slot_covered(dt, slot), do: false

  def cover?(%Slot{} = slot, %Slot{from: from, to: to}, true)
      when is_slot_border(from, slot) or is_slot_border(to, slot),
      do: false

  def cover?(%Slot{}, %Slot{}, _), do: true
  def cover?(%Slot{} = slot, origin, strict?), do: cover?(slot, wrap(origin), strict?)

  @spec disjoint?(s1 :: origin(), s2 :: origin()) :: boolean()
  @doc """
  Returns `true` if two slots are disjoined, `false` otherwise.

  ## Examples

      iex> slot = %Tempus.Slot{from: ~U|2015-09-01 00:00:00Z|, to: ~U|2015-10-01 00:00:00Z|}
      iex> inner = %Tempus.Slot{from: ~U|2015-09-01 00:00:00Z|, to: ~U|2015-09-01 01:00:00Z|}
      iex> Tempus.Slot.disjoint?(slot, inner)
      false
      iex> outer = %Tempus.Slot{from: ~U|2015-10-01 00:00:01Z|, to: ~U|2015-10-01 01:00:00Z|}
      iex> Tempus.Slot.disjoint?(slot, outer)
      true
      iex> Tempus.Slot.disjoint?(~D|2000-01-01|, ~U|2015-10-01 00:00:01Z|)
      true
  """
  def disjoint?(%Slot{} = s1, %Slot{} = s2) when is_joint(s1, s2), do: false
  def disjoint?(%Slot{}, %Slot{}), do: true
  def disjoint?(s1, s2), do: disjoint?(wrap(s1), wrap(s2))

  @doc """
  Returns `true` if two slots are neighbours (touch at a boundary), `false` otherwise.

  ## Examples

      iex> slot = Tempus.Slot.wrap(~D|2015-09-01|)
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-09-02|))
      true
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-08-31|))
      true
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-09-03|))
      false
  """
  @spec neighbour?(s1 :: origin(), s2 :: origin()) :: boolean()
  def neighbour?(s1, s2) do
    s1 = wrap(s1)
    s2 = wrap(s2)

    (not is_nil(s1.to) and not is_nil(s2.from) and is_datetime_equal(s1.to, s2.from)) or
      (not is_nil(s2.to) and not is_nil(s1.from) and is_datetime_equal(s2.to, s1.from))
  end

  @spec intersect(slots :: Enum.t()) :: Slot.t() | nil
  @doc """
  Intersects slots to the minimal covered timeslice.

  ### Example

      iex> Tempus.Slot.intersect([Tempus.Slot.id(), Tempus.Slot.id()])
      Tempus.Slot.id()

      iex> Tempus.Slot.intersect([%Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]},
      ...>   %Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]}])
      %Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z], from_open: true, to_open: true}

      iex> Tempus.Slot.intersect([~D|2020-09-30|, Tempus.Slot.id()])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-01 00:00:00.000000Z], from_open: false, to_open: true}
  """
  def intersect(slots) do
    Enum.reduce(slots, fn
      _slot, nil ->
        nil

      slot, void() ->
        wrap(slot)

      void(), slot ->
        wrap(slot)

      slot, acc ->
        slot = wrap(slot)
        acc = wrap(acc)

        if disjoint?(acc, slot) do
          nil
        else
          {from, fo} = intersect_from(slot, acc)
          {to, to_op} = intersect_to(slot, acc)
          %Slot{from: from, to: to, from_open: fo, to_open: to_op}
        end
    end)
  end

  @spec intersect_from(Slot.t(), Slot.t()) :: {DateTime.t() | nil, boolean()}
  defp intersect_from(%Slot{from: nil}, %Slot{from: nil}), do: {nil, true}
  defp intersect_from(%Slot{from: f1, from_open: fo1}, %Slot{from: nil}), do: {f1, fo1}
  defp intersect_from(%Slot{from: nil}, %Slot{from: f2, from_open: fo2}), do: {f2, fo2}

  defp intersect_from(%Slot{from: f1, from_open: fo1}, %Slot{from: f2, from_open: fo2}) do
    case DateTime.compare(f1, f2) do
      :gt -> {f1, fo1}
      :lt -> {f2, fo2}
      :eq -> {f1, fo1 or fo2}
    end
  end

  @spec intersect_to(Slot.t(), Slot.t()) :: {DateTime.t() | nil, boolean()}
  defp intersect_to(%Slot{to: nil}, %Slot{to: nil}), do: {nil, true}
  defp intersect_to(%Slot{to: t1, to_open: to1}, %Slot{to: nil}), do: {t1, to1}
  defp intersect_to(%Slot{to: nil}, %Slot{to: t2, to_open: to2}), do: {t2, to2}

  defp intersect_to(%Slot{to: t1, to_open: to1}, %Slot{to: t2, to_open: to2}) do
    case DateTime.compare(t1, t2) do
      :lt -> {t1, to1}
      :gt -> {t2, to2}
      :eq -> {t1, to1 or to2}
    end
  end

  @spec join(slots :: Enum.t()) :: Slot.t()
  @doc """
  Joins slots to the maximal covered timeslice.

  ### Example

      iex> Tempus.Slot.join([])
      Tempus.Slot.id()

      iex> Tempus.Slot.join([Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|)])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-03 00:00:00.000000Z], from_open: false, to_open: true}

      iex> Tempus.Slot.join([~D|2020-09-30|, ~D|2020-10-02|])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-03 00:00:00.000000Z], from_open: false, to_open: true}
  """
  def join([]), do: void()
  def join([slot | slots]), do: do_join(slots, wrap(slot))

  defp do_join([], acc), do: acc
  defp do_join(any, void()), do: join(any)
  defp do_join([void() | slots], acc), do: do_join(slots, acc)

  defp do_join([slot | slots], acc) do
    slot = wrap(slot)

    {from, from_open} =
      cond do
        is_nil(slot.from) or is_nil(acc.from) ->
          {nil, true}

        DateTime.compare(slot.from, acc.from) == :lt ->
          {slot.from, slot.from_open}

        DateTime.compare(slot.from, acc.from) == :gt ->
          {acc.from, acc.from_open}

        true ->
          {slot.from, slot.from_open and acc.from_open}
      end

    {to, to_open} =
      cond do
        is_nil(slot.to) or is_nil(acc.to) ->
          {nil, true}

        DateTime.compare(slot.to, acc.to) == :gt ->
          {slot.to, slot.to_open}

        DateTime.compare(slot.to, acc.to) == :lt ->
          {acc.to, acc.to_open}

        true ->
          {slot.to, slot.to_open and acc.to_open}
      end

    do_join(slots, %Slot{from: from, to: to, from_open: from_open, to_open: to_open})
  end

  @spec join(Slot.t(), Slot.t()) :: Slot.t()
  @doc """
  Joins two slots to the maximal covered timeslice.

  ### Example

      iex> Tempus.Slot.join(Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|))
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-03 00:00:00.000000Z], from_open: false, to_open: true}

      iex> Tempus.Slot.join(~D|2020-09-30|, ~D|2020-10-02|)
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-03 00:00:00.000000Z], from_open: false, to_open: true}
  """
  def join(s1, s2), do: join([s1, s2])

  @spec duration(slot :: Slot.origin(), unit :: System.time_unit()) ::
          non_neg_integer() | :infinity
  @doc """
  Calculates the duration of a slot in units given as a second parameter
    (default: `:second`.)

  ### Example

      iex> Tempus.Slot.duration(~D|2020-09-03|)
      86400
      iex> Tempus.Slot.duration(Tempus.Slot.id())
      0
      iex> Tempus.Slot.duration(%Tempus.Slot{from: nil, to: DateTime.utc_now()})
      :infinity
      iex> Tempus.Slot.duration(%Tempus.Slot{from: DateTime.utc_now(), to: nil})
      :infinity
  """
  def duration(slot, unit \\ :second)
  def duration(slot, unit) when not is_struct(slot, Slot), do: slot |> wrap() |> duration(unit)
  def duration(void(), _), do: 0
  def duration(%Slot{from: nil, to: %DateTime{}}, _), do: :infinity
  def duration(%Slot{from: %DateTime{}, to: nil}, _), do: :infinity

  def duration(%Slot{from: %DateTime{} = from, to: %DateTime{} = to}, unit),
    do: DateTime.diff(to, from, unit)

  @spec compare(s1 :: origin(), s2 :: origin(), strict :: boolean()) :: :lt | :gt | :eq | :joint
  @doc """
  Compares two slot structs.

  Returns `:gt` if first slot is strictly later than the second and `:lt` for vice versa.
  **NB** `:eq` is returned not only if slots are equal, but also when they are overlapped.

  Might be used in `Enum.sort/2`.

  ### Examples

      iex> slot = Tempus.Slot.wrap(~D[2020-09-30])
      iex> slot1 = %Tempus.Slot{from: nil, to: ~U[2020-09-30 00:00:00.000000Z]}
      iex> slot2 = %Tempus.Slot{from: nil, to: DateTime.utc_now()}
      iex> slot3 = %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: nil}
      iex> slot4 = %Tempus.Slot{from: DateTime.utc_now(), to: nil}
      iex> Tempus.Slot.compare(Tempus.Slot.id(), Tempus.Slot.id(), true)
      :eq
      iex> Tempus.Slot.compare(slot1, slot2, false)
      :eq
      iex> Tempus.Slot.compare(slot1, slot2, true)
      :joint
      iex> Tempus.Slot.compare(slot3, slot4, false)
      :eq
      iex> Tempus.Slot.compare(slot3, slot4, true)
      :joint
      iex> Tempus.Slot.compare(slot, slot, true)
      :eq
      iex> Tempus.Slot.compare(slot, DateTime.utc_now(), true)
      :lt
      iex> Tempus.Slot.compare(slot, ~D|2000-01-01|, true)
      :gt
      iex> Tempus.Slot.compare(slot, slot.from, true)
      :joint
  """
  def compare(s1, s2, strict \\ false)

  def compare(value, value, _), do: :eq
  def compare(_, void(), false), do: :eq
  def compare(_, void(), true), do: :joint
  def compare(void(), _, false), do: :eq
  def compare(void(), _, true), do: :joint
  def compare(nil, _, _), do: :lt
  def compare(_, nil, _), do: :lt
  def compare(%Slot{} = s1, %Slot{} = s2, _) when is_slot_coming_before(s1, s2), do: :lt
  def compare(%Slot{} = s1, %Slot{} = s2, _) when is_slot_coming_before(s2, s1), do: :gt

  def compare(%Date{} = d, %DateTime{} = dt, strict),
    do: compare(Slot.wrap(d), Slot.wrap(dt), strict)

  def compare(%DateTime{} = dt, %Date{} = d, strict),
    do: compare(Slot.wrap(dt), Slot.wrap(d), strict)

  def compare(%Date{} = d, %Slot{} = s, strict), do: compare(Slot.wrap(d), s, strict)
  def compare(%Slot{} = s, %Date{} = d, strict), do: compare(s, Slot.wrap(d), strict)
  def compare(%DateTime{} = dt, %Slot{} = s, strict), do: compare(Slot.wrap(dt), s, strict)
  def compare(%Slot{} = s, %DateTime{} = dt, strict), do: compare(s, Slot.wrap(dt), strict)

  def compare(%Slot{from: nil, to: %DateTime{}}, %Slot{from: nil, to: %DateTime{}}, false),
    do: :eq

  def compare(
        %Slot{from: nil, to: %DateTime{} = t1},
        %Slot{from: nil, to: %DateTime{} = t2},
        true
      ),
      do: if(DateTime.compare(t1, t2) == :eq, do: :eq, else: :joint)

  def compare(%Slot{from: %DateTime{}, to: nil}, %Slot{from: %DateTime{}, to: nil}, false),
    do: :eq

  def compare(
        %Slot{from: %DateTime{} = f1, to: nil},
        %Slot{from: %DateTime{} = f2, to: nil},
        true
      ),
      do: if(DateTime.compare(f1, f2) == :eq, do: :eq, else: :joint)

  def compare(%Slot{from: f1, to: t1}, %Slot{from: f2, to: t2}, strict) do
    f2l = t1 && f2 && DateTime.compare(t1, f2)
    l2f = f1 && t2 && DateTime.compare(f1, t2)

    case {strict, f2l, l2f} do
      {_, :lt, _} ->
        :lt

      {_, _, :gt} ->
        :gt

      {false, _, _} ->
        :eq

      {true, nil, _} ->
        :joint

      {true, _, nil} ->
        :joint

      {true, _, _} ->
        if DateTime.compare(f1, f2) == :eq && DateTime.compare(t1, t2) == :eq,
          do: :eq,
          else: :joint
    end
  end

  def compare(s1, s2, strict), do: compare(wrap(s1), wrap(s2), strict)

  @spec strict_compare(s1 :: Slot.origin(), s2 :: Slot.origin()) :: :eq | :lt | :gt | :joint
  @doc """
  Compares two slot structs. The same as `compare/2`, but returns `:joint` if
  the slots are overlapped.

  ### Examples

      iex> Tempus.Slot.strict_compare(~D|2020-01-01|, DateTime.utc_now())
      :lt
  """
  def strict_compare(s1, s2) when is_origin(s1) and is_origin(s2),
    do: compare(s1, s2, true)

  @spec wrap(origin(), DateTime.t()) :: Slot.t()
  @doc """
  Wraps the argument into a slot. For `DateTime` it'd be a point slot.
  For a `Date`, it would be the whole day $[00:00:00.000000 \to 00:00:00.000000\text{ next day})$.

  ## Examples

      iex> Tempus.Slot.wrap(~D|2020-08-06|)
      %Tempus.Slot{from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-07 00:00:00.000000Z], from_open: false, to_open: true}
      iex> Tempus.Slot.wrap(:ok)
      Tempus.Slot.id()
  """
  def wrap(moment \\ nil, origin \\ DateTime.utc_now())

  def wrap(nil, origin), do: wrap(DateTime.utc_now(), origin)
  def wrap(%Slot{} = slot, _), do: slot
  def wrap(%DateTime{} = dt, _), do: %Slot{from: dt, to: dt, from_open: false, to_open: false}

  def wrap(
        %Time{
          calendar: calendar,
          hour: hour,
          microsecond: microsecond,
          minute: minute,
          second: second
        },
        origin
      ) do
    wrap(%DateTime{
      calendar: calendar,
      day: origin.day,
      hour: hour,
      microsecond: microsecond,
      minute: minute,
      month: origin.month,
      second: second,
      std_offset: origin.std_offset,
      time_zone: origin.time_zone,
      utc_offset: origin.utc_offset,
      year: origin.year,
      zone_abbr: origin.zone_abbr
    })
  end

  def wrap(%Date{calendar: calendar, day: day, month: month, year: year} = date, origin) do
    next_date = Date.add(date, 1)

    %Slot{
      from: %DateTime{
        calendar: calendar,
        day: day,
        hour: 0,
        microsecond: {0, 6},
        minute: 0,
        month: month,
        second: 0,
        std_offset: origin.std_offset,
        time_zone: origin.time_zone,
        utc_offset: origin.utc_offset,
        year: year,
        zone_abbr: origin.zone_abbr
      },
      to: %DateTime{
        calendar: calendar,
        day: next_date.day,
        hour: 0,
        microsecond: {0, 6},
        minute: 0,
        month: next_date.month,
        second: 0,
        std_offset: origin.std_offset,
        time_zone: origin.time_zone,
        utc_offset: origin.utc_offset,
        year: next_date.year,
        zone_abbr: origin.zone_abbr
      },
      from_open: false,
      to_open: true
    }
  end

  def wrap(_, _), do: void()

  @doc false
  @spec shift(
          slot :: t(),
          action :: [
            {:to, integer()} | {:from, integer()} | {:by, integer()} | {:unit, System.time_unit()}
          ]
        ) :: Slot.t()
  def shift(%Slot{from: from, to: to, from_open: fo, to_open: to_op}, action \\ []) do
    {multiplier, unit} =
      case Keyword.get(action, :unit, :microsecond) do
        :day -> {60 * 60 * 24 * 1_000_000, :microsecond}
        :hour -> {60 * 60 * 1_000_000, :microsecond}
        :minute -> {60 * 1_000_000, :microsecond}
        other -> {1, other}
      end

    [by_from, by_to] =
      action
      |> Keyword.get(:by)
      |> case do
        nil -> Enum.map([:from, :to], &Keyword.get(action, &1, 0))
        value -> [value, value]
      end
      |> Enum.map(&(&1 * multiplier))

    check_shifted(do_shift(from, by_from, unit), do_shift(to, by_to, unit), fo, to_op)
  end

  @spec check_shifted(maybe_datetime, maybe_datetime, boolean(), boolean()) :: Slot.t()
        when maybe_datetime: nil | DateTime.t()
  defp check_shifted(nil, nil, _fo, _to_op),
    do: %Slot{from: nil, to: nil, from_open: true, to_open: true}

  defp check_shifted(nil, to, _fo, to_op),
    do: %Slot{from: nil, to: to, from_open: true, to_open: to_op}

  defp check_shifted(from, nil, fo, _to_op),
    do: %Slot{from: from, to: nil, from_open: fo, to_open: true}

  defp check_shifted(%DateTime{} = from, %DateTime{} = to, fo, to_op) do
    if DateTime.compare(to, from) == :lt do
      void()
    else
      %Slot{from: from, to: to, from_open: fo, to_open: to_op}
    end
  end

  @spec do_shift(maybe_datetime, integer(), System.time_unit()) :: maybe_datetime
        when maybe_datetime: nil | DateTime.t()
  defp do_shift(nil, _, _), do: nil

  defp do_shift(%DateTime{microsecond: {_, 0}} = dt, count, unit),
    do:
      %DateTime{dt | microsecond: {0, 6}}
      |> DateTime.truncate(unit)
      |> DateTime.add(count, unit)

  defp do_shift(%DateTime{microsecond: {value, n}} = dt, count, unit),
    do:
      %DateTime{dt | microsecond: {:erlang.rem(value, round(:math.pow(10, n))), Enum.max([6, 6])}}
      |> DateTime.truncate(unit)
      |> DateTime.add(count, unit)

  @spec shift_tz(
          slot :: Slot.t(),
          tz :: Calendar.time_zone(),
          tz_db :: Calendar.time_zone_database()
        ) :: Slot.t()
  @doc """
  Shifts both `from` and `to` values to `UTC` zone.

  ### Examples

  ```elixir
  slot = %Tempus.Slot{
     from: DateTime.from_naive!(~N|2018-01-05 21:00:00|, "America/New_York"),
     to: DateTime.from_naive!(~N|2018-01-08 08:59:59|, "Australia/Sydney")
  }
  #⇒ %Tempus.Slot{from: ~U[2018-01-06 02:00:00Z], to: ~U[2018-01-07 21:59:59Z], from_open: false, to_open: true}
  ```
  """
  def shift_tz(
        %Slot{from: from, to: to, from_open: fo, to_open: to_op},
        tz \\ "Etc/UTC",
        tz_db \\ Calendar.get_time_zone_database()
      ) do
    %Slot{
      from: from && DateTime.shift_zone!(from, tz, tz_db),
      to: to && DateTime.shift_zone!(to, tz, tz_db),
      from_open: fo,
      to_open: to_op
    }
  end

  @spec gap([t()]) :: t()
  @doc false
  def gap([
        %Slot{to: from, to_open: from_open} = prev,
        %Slot{from: to, from_open: to_open} = next
      ])
      when is_slot_coming_before(prev, next) do
    %Slot{from: from, to: to, from_open: not from_open, to_open: not to_open}
  end

  def gap([%Slot{} = prev, %Slot{} = next]) when is_slot_coming_before(next, prev),
    do: gap([next, prev])

  def gap([%Slot{from: nil, to: from, to_open: from_open}]),
    do: %Slot{from: from, to: nil, from_open: not from_open, to_open: true}

  def gap([%Slot{from: to, from_open: to_open, to: nil}]),
    do: %Slot{from: nil, to: to, from_open: true, to_open: not to_open}

  def gap(_), do: void()

  defimpl Inspect do
    @moduledoc false

    import Inspect.Algebra
    @fancy_inspect Application.compile_env(:tempus, :inspect, :sigil)

    defp open_bracket(true), do: "("
    defp open_bracket(false), do: "["
    defp close_bracket(true), do: ")"
    defp close_bracket(false), do: "]"

    defp value(from, to, _opts) do
      Enum.map_join([from, to], " → ", fn
        nil -> "∞"
        dt -> DateTime.to_iso8601(dt)
      end)
    end

    def inspect(
          %Tempus.Slot{from: from, to: to, from_open: fo, to_open: to_op},
          %Inspect.Opts{custom_options: [_ | _]} = opts
        ) do
      opts.custom_options
      |> Keyword.get(:fancy, @fancy_inspect)
      |> case do
        truthy when truthy in [:emoji, true] ->
          tag =
            case truthy do
              :emoji -> "⌚"
              true -> "𝕥"
            end

          concat([tag, open_bracket(fo), value(from, to, opts), close_bracket(to_op)])

        false ->
          concat([
            "#Slot<",
            to_doc([from: from, to: to, from_open: fo, to_open: to_op], opts),
            ">"
          ])

        :sigil ->
          case {from, to} do
            {nil, nil} -> "%Tempus.Slot{}"
            {from, nil} -> "%Tempus.Slot{from: " <> inspect(from) <> "}"
            {nil, to} -> "%Tempus.Slot{to: " <> inspect(to) <> "}"
            {from, to} -> "~I" <> open_bracket(fo) <> "#{from}|#{to}" <> close_bracket(to_op)
          end
      end
    end

    def inspect(%Tempus.Slot{from: from, to: to, from_open: fo, to_open: to_op}, opts) do
      concat(["~I", open_bracket(fo), value(from, to, opts), close_bracket(to_op)])
    end
  end
end
