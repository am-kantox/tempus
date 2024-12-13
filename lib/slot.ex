defmodule Tempus.Slot do
  @moduledoc """
  Declares a timeslot and exports functions to check whether the given date
    and/or datetime is covered by this slot or not.

  This module probably should not be called directly.
  """
  alias __MODULE__

  import Tempus.Guards

  @typedoc "A timeslot to be used in `Tempus`"
  @type t :: %__MODULE__{
          from: nil | DateTime.t(),
          to: nil | DateTime.t()
        }

  @typedoc "The origin used in comparisons and calculations"
  @type origin :: Slot.t() | Date.t() | DateTime.t() | nil

  defstruct [:from, :to]

  @spec new([{:from, origin()} | {:to, origin()}]) :: {:ok, t()} | {:error, any()}
  @doc """
  Creates new slot using `arg[:from]` as a starting origin and `arg[:to]` and an ending origin.

  ## Examples

      iex> Tempus.Slot.new(from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|)
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|}}
      iex> Tempus.Slot.new(%{from: ~D|2015-09-30|, to: ~U|2015-10-01T12:00:00Z|})
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00.000000Z|, to: ~U|2015-10-01 12:00:00Z|}}
  """
  def new(from_to), do: new(from_to[:from], from_to[:to])

  @spec new(from :: origin(), to :: origin()) :: {:ok, t()} | {:error, any()}
  @doc """
  Creates new slot using `from` as a starting origin and `to` and an ending origin.
  See `new/1` for more readable implementation.

  ## Examples

      iex> import Tempus.Sigils
      iex> Tempus.Slot.new(~U|2015-09-30 00:00:00Z|, ~U|2015-10-01 01:00:00Z|)
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|}}
      iex> Tempus.Slot.new(~D|2015-09-30|, ~U|2015-10-01T12:00:00Z|)
      {:ok, %Tempus.Slot{from: ~U|2015-09-30 00:00:00.000000Z|, to: ~U|2015-10-01 12:00:00Z|}}
      iex> Tempus.Slot.new(nil, nil)
      {:ok, Tempus.Slot.id()}
      iex> Tempus.Slot.new(~D|2015-09-30|, nil)
      {:ok, ~I(2015-09-30T00:00:00.000000Z → ∞)un}
      iex> Tempus.Slot.new(nil, ~D|2015-09-30|)
      {:ok, ~I(∞ → 2015-09-30T23:59:59.999999Z)nu}
      iex> Tempus.Slot.new(:ok, :ok)
      {:error, :invalid_input}
  """
  def new(from, to) when not is_origin(from) when not is_origin(to), do: {:error, :invalid_input}
  def new(nil, nil), do: {:ok, %Tempus.Slot{from: nil, to: nil}}
  def new(from, nil), do: {:ok, %Tempus.Slot{from: wrap(from).from, to: nil}}
  def new(nil, to), do: {:ok, %Tempus.Slot{from: nil, to: wrap(to).to}}
  def new(from, to), do: {:ok, [from, to] |> Enum.map(&wrap/1) |> join()}

  @spec new!(from :: origin(), to :: origin()) :: t() | no_return

  @doc """
  Creates new slot using `from` as a starting origin and `to` and an ending origin.
  Unlike `new/1`, this function raises on malformed input.

  ## Examples

      iex> Tempus.Slot.new!(~U|2015-09-30 00:00:00Z|, ~U|2015-10-01 01:00:00Z|)
      %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|}
      iex> Tempus.Slot.new!(~D|2015-09-30|, ~U|2015-10-01T12:00:00Z|)
      %Tempus.Slot{from: ~U|2015-09-30 00:00:00.000000Z|, to: ~U|2015-10-01 12:00:00Z|}
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
      %Slot{from: nil, to: nil}
    end
  end

  @doc "Identity element, void slot `~I[nil → nil]`"
  @spec id :: Slot.t()
  def id, do: void()

  @spec valid?(slot :: Slot.t()) :: boolean()
  @doc """
  Checks whether the `Slot` is valid (to > from) or not.

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
  def valid?(%Slot{from: nil, to: %DateTime{}}), do: true
  def valid?(%Slot{from: %DateTime{}, to: nil}), do: true

  def valid?(%Slot{from: %DateTime{} = from, to: %DateTime{} = to}),
    do: DateTime.compare(from, to) != :gt

  def valid?(_), do: false

  @doc """
  Splits the slot given asa first argument to two on borders given as a second slot.

  ## Examples

      iex> outer = Tempus.Slot.wrap(~D[2023-04-12])
      ...> {:ok, inner} = Tempus.Slot.new(~U[2023-04-12 12:00:00Z], ~U[2023-04-12 13:00:00Z])
      iex> Tempus.Slot.xor(outer, inner)
      [%Tempus.Slot{from: ~U[2023-04-12 00:00:00.000000Z], to: ~U[2023-04-12 12:00:00Z]},
       %Tempus.Slot{from: ~U[2023-04-12 13:00:00Z], to: ~U[2023-04-12 23:59:59.999999Z]}]
      iex> Tempus.Slot.xor(outer, inner) == Tempus.Slot.xor(inner, outer)
      true
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

  def xor(outer, inner), do: [Slot.new!(outer.from, inner.from), Slot.new!(inner.to, outer.to)]

  @spec cover?(slot :: Slot.t(), dt :: origin(), strict? :: boolean()) ::
          boolean()
  @doc """
  Checks whether to `Slot` covers the data/datetime passed as a second argument.

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
      true
      iex> Tempus.Slot.cover?(slot, dt_to, true)
      false
      iex> Tempus.Slot.cover?(slot, d_from)
      true
      iex> Tempus.Slot.cover?(slot, d_from, true)
      false
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
      iex> inner = %Tempus.Slot{from: ~U|2015-09-01 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|}
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
  def disjoint?(s1, s2), do: [s1, s2] |> Enum.map(&wrap/1) |> Enum.reduce(&disjoint?/2)

  @doc """
  Returns `true` if two slots are neighbours, `false` otherwise.

  ## Examples

      iex> slot = %Tempus.Slot{from: ~U|2015-09-01 00:00:00Z|, to: ~U|2015-10-01 23:59:59Z|}
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-10-02|))
      true
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-08-31|))
      true
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-10-01|))
      false
      iex> Tempus.Slot.neighbour?(slot, Tempus.Slot.wrap(~D|2015-10-03|))
      false
  """
  @spec neighbour?(s1 :: origin(), s2 :: origin()) :: boolean()
  def neighbour?(s1, s2) do
    [%Slot{to: to}, %Slot{from: from}] = [s1, s2] |> Enum.map(&wrap/1) |> Enum.sort(Slot)

    not is_nil(to) and not is_nil(from) and DateTime.compare(from, to) == :gt and
      DateTime.diff(from, to, :second) <= 1
  end

  @spec intersect(slots :: Enum.t()) :: Slot.t() | nil
  @doc """
  Intersects slots to the minimal covered timeslice.

  ### Example

      iex> Tempus.Slot.intersect([Tempus.Slot.id(), Tempus.Slot.id()])
      Tempus.Slot.id()

      iex> Tempus.Slot.intersect([%Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]},
      ...>   %Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]}])
      %Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]}

      iex> Tempus.Slot.intersect([%Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: nil},
      ...>   %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: nil}])
      %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: nil}

      iex> Tempus.Slot.intersect([~D|2020-09-30|, Tempus.Slot.id()])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-09-30 23:59:59.999999Z]}

      iex> Tempus.Slot.intersect([Tempus.Slot.id(), ~D|2020-09-30|])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-09-30 23:59:59.999999Z]}

      iex> Tempus.Slot.intersect([~D|2020-09-30|,
      ...>   %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: nil}])
      %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: ~U[2020-09-30 23:59:59.999999Z]}

      iex> Tempus.Slot.intersect([~D|2020-09-30|,
      ...>   %Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]}])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-09-30 23:00:00Z]}

      iex> Tempus.Slot.intersect([
      ...>   %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: nil}, ~D|2020-09-30|])
      %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: ~U[2020-09-30 23:59:59.999999Z]}

      iex> Tempus.Slot.intersect([
      ...>   %Tempus.Slot{from: nil, to: ~U[2020-09-30 23:00:00Z]}, ~D|2020-09-30|])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-09-30 23:00:00Z]}

      iex> Tempus.Slot.intersect([Tempus.Slot.wrap(~D|2020-09-30|),
      ...>   %Tempus.Slot{from: ~U|2020-09-30 23:00:00Z|, to: ~U|2020-10-02 00:00:00Z|}])
      %Tempus.Slot{from: ~U[2020-09-30 23:00:00Z], to: ~U[2020-09-30 23:59:59.999999Z]}

      iex> Tempus.Slot.intersect([~D|2020-09-30|, ~D|2000-09-30|,
      ...>   %Tempus.Slot{from: ~U|2020-09-30 23:00:00Z|, to: ~U|2020-10-02 00:00:00Z|}])
      nil
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

        if disjoint?(acc, slot),
          do: nil,
          else: %Slot{from: intersect_from(slot, acc), to: intersect_to(slot, acc)}
    end)
  end

  @spec intersect_from(Slot.t(), Slot.t()) :: DateTime.t() | nil
  defp intersect_from(%Slot{from: nil}, %Slot{from: nil}), do: nil
  defp intersect_from(%Slot{from: f1}, %Slot{from: nil}), do: f1
  defp intersect_from(%Slot{from: nil}, %Slot{from: f2}), do: f2
  defp intersect_from(%Slot{from: f1}, %Slot{from: f2}), do: Enum.max([f1, f2], DateTime)

  @spec intersect_to(Slot.t(), Slot.t()) :: DateTime.t() | nil
  defp intersect_to(%Slot{to: nil}, %Slot{to: nil}), do: nil
  defp intersect_to(%Slot{to: t1}, %Slot{to: nil}), do: t1
  defp intersect_to(%Slot{to: nil}, %Slot{to: t2}), do: t2
  defp intersect_to(%Slot{to: t1}, %Slot{to: t2}), do: Enum.min([t1, t2], DateTime)

  @spec join(slots :: Enum.t()) :: Slot.t()
  @doc """
  Joins slots to the maximal covered timeslice.

  ### Example

      iex> Tempus.Slot.join([])
      Tempus.Slot.id()

      iex> Tempus.Slot.join([Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|)])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]}

      iex> Tempus.Slot.join([~D|2020-09-30|, ~D|2020-10-02|])
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]}
  """
  def join([]), do: void()
  def join([slot | slots]), do: do_join(slots, wrap(slot))

  defp do_join([], acc), do: acc
  defp do_join(any, void()), do: join(any)
  defp do_join([void() | slots], acc), do: do_join(slots, acc)

  defp do_join([slot | slots], acc) do
    slot = wrap(slot)

    from =
      if not is_nil(slot.from) and not is_nil(acc.from) do
        if DateTime.compare(slot.from, acc.from) == :lt,
          do: slot.from,
          else: acc.from
      end

    to =
      if not is_nil(slot.to) and not is_nil(acc.to) do
        if DateTime.compare(slot.to, acc.to) == :gt,
          do: slot.to,
          else: acc.to
      end

    do_join(slots, %Slot{from: from, to: to})
  end

  @spec join(Slot.t(), Slot.t()) :: Slot.t()
  @doc """
  Joins two slots to the maximal covered timeslice.

  ### Example

      iex> Tempus.Slot.join(Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|))
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]}

      iex> Tempus.Slot.join(~D|2020-09-30|, ~D|2020-10-02|)
      %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]}
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
    do: to |> DateTime.add(1, unit) |> DateTime.diff(from, unit)

  @spec compare(s1 :: origin(), s2 :: origin(), strict :: boolean()) :: :lt | :gt | :eq | :joint
  @doc """
  Compares two slot structs.

  Returns `:gt` if first slot is strictly later than the second and `:lt` for vice versa.
  **NB** `:eq` is returned not only if slots are equal, but also when they are overlapped.

  Might be used in `Enum.sort/2`.

  ### Examples

      iex> slot = %Tempus.Slot{from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]}
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
  Wraps the argument into a slot. For `DateTime` it’d be a single microsecond.
  For a `Date`, it would be the whole day, starting at `00:00:00.000000` and
      ending at `23:59:59:999999`.

  ## Examples

      iex> Tempus.Slot.wrap(~D|2020-08-06|)
      %Tempus.Slot{from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]}
      iex> Tempus.Slot.wrap(:ok)
      Tempus.Slot.id()
  """
  def wrap(moment \\ nil, origin \\ DateTime.utc_now())

  def wrap(nil, origin), do: wrap(DateTime.utc_now(), origin)
  def wrap(%Slot{} = slot, _), do: slot
  def wrap(%DateTime{} = dt, _), do: %Slot{from: dt, to: dt}

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

  def wrap(%Date{calendar: calendar, day: day, month: month, year: year}, origin) do
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
        day: day,
        hour: 23,
        microsecond: {999_999, 6},
        minute: 59,
        month: month,
        second: 59,
        std_offset: origin.std_offset,
        time_zone: origin.time_zone,
        utc_offset: origin.utc_offset,
        year: year,
        zone_abbr: origin.zone_abbr
      }
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
  def shift(%Slot{from: from, to: to}, action \\ []) do
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

    check_shifted(do_shift(from, by_from, unit), do_shift(to, by_to, unit))
  end

  @spec check_shifted(maybe_datetime, maybe_datetime) :: Slot.t()
        when maybe_datetime: nil | DateTime.t()
  defp check_shifted(nil, nil), do: void()
  defp check_shifted(nil, to), do: %Slot{from: nil, to: to}
  defp check_shifted(from, nil), do: %Slot{from: from, to: nil}

  defp check_shifted(%DateTime{} = from, %DateTime{} = to)
       when not is_datetime_coming_before(to, from),
       do: %Slot{from: from, to: to}

  defp check_shifted(_, _), do: void()

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
  #⇒ %Tempus.Slot{from: ~U[2018-01-06 02:00:00Z], to: ~U[2018-01-07 21:59:59Z]}
  ```
  """
  def shift_tz(
        %Slot{from: from, to: to},
        tz \\ "Etc/UTC",
        tz_db \\ Calendar.get_time_zone_database()
      ) do
    %Slot{from: DateTime.shift_zone!(from, tz, tz_db), to: DateTime.shift_zone!(to, tz, tz_db)}
  end

  @spec gap([t()]) :: t()
  @doc false
  def gap([%Slot{to: from} = prev, %Slot{from: to} = next])
      when is_slot_coming_before(prev, next),
      do: shift(%Slot{from: from, to: to}, from: 1, to: -1)

  def gap([%Slot{} = prev, %Slot{} = next]) when is_slot_coming_before(next, prev),
    do: gap([next, prev])

  def gap([%Slot{from: nil, to: from}]), do: shift(%Slot{from: from, to: nil}, from: 1)
  def gap([%Slot{from: to, to: nil}]), do: shift(%Slot{from: nil, to: to}, to: -1)
  def gap(_), do: void()

  defimpl Inspect do
    @moduledoc false

    import Inspect.Algebra
    @fancy_inspect Application.compile_env(:tempus, :inspect, :sigil)

    defp value(from, to, _opts) do
      Enum.map_join([from, to], " → ", fn
        nil -> "∞"
        dt -> DateTime.to_iso8601(dt)
      end)
    end

    def inspect(%Tempus.Slot{from: from, to: to}, %Inspect.Opts{custom_options: [_ | _]} = opts) do
      opts.custom_options
      |> Keyword.get(:fancy, @fancy_inspect)
      |> case do
        truthy when truthy in [:emoji, true] ->
          tag =
            case truthy do
              :emoji -> "⌚"
              true -> "𝕥"
            end

          concat([tag, "(", value(from, to, opts), ")"])

        false ->
          concat(["#Slot<", to_doc([from: from, to: to], opts), ">"])

        :sigil ->
          case {from, to} do
            {nil, nil} -> "%Tempus.Slot{}"
            {from, nil} -> "%Tempus.Slot{from: " <> inspect(from) <> "}"
            {nil, to} -> "%Tempus.Slot{to: " <> inspect(to) <> "}"
            {from, to} -> "~I[#{from}|#{to}]"
          end
      end
    end

    def inspect(%Tempus.Slot{from: from, to: to}, opts) do
      concat(["~I(", value(from, to, opts), ")"])
    end
  end
end
