defmodule Tempus.Slot do
  @moduledoc """
  Declares a timeslot and exports functions to check whether the given date
    and/or datetime is covered by this slot or not.

  This module probably should not be called directly.
  """
  alias __MODULE__

  @typedoc "A timeslot to be used in `Tempus`"
  @type t :: %__MODULE__{
          from: DateTime.t(),
          to: DateTime.t()
        }

  @typedoc "The origin used in comparisons and calculations"
  @type origin :: Slot.t() | Date.t() | DateTime.t() | nil

  defstruct [:from, :to]

  @spec valid?(slot :: Slot.t()) :: boolean()
  @doc """
  Checks whether the `Slot` is valid (to > from) or not.

  ## Examples

      iex> slot = %Tempus.Slot{from: ~U|2015-09-30 00:00:00Z|, to: ~U|2015-10-01 01:00:00Z|}
      iex> Tempus.Slot.valid?(slot)
      true
      iex> Tempus.Slot.valid?(%Tempus.Slot{from: slot.to, to: slot.from})
      false
  """
  def valid?(%Slot{from: from, to: to}),
    do: DateTime.compare(from, to) != :gt

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
      iex> Tempus.Slot.cover?(slot, d_to)
      false
  """
  def cover?(slot, dt, strict? \\ false)

  def cover?(%Slot{from: from, to: to}, %DateTime{} = dt, true),
    do: DateTime.compare(from, dt) == :lt and DateTime.compare(to, dt) == :gt

  def cover?(%Slot{from: from, to: to}, %DateTime{} = dt, false),
    do: DateTime.compare(from, dt) in [:lt, :eq] and DateTime.compare(to, dt) in [:gt, :eq]

  def cover?(%Slot{} = slot, %Date{} = dt, strict?),
    do: cover?(slot, wrap(dt, slot.from), strict?)

  def cover?(%Slot{} = slot, %Slot{from: from, to: to}, strict?),
    do: cover?(slot, from, strict?) and cover?(slot, to, strict?)

  @spec disjoint?(s1 :: Slot.t(), s2 :: Slot.t()) :: boolean()
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
  """
  def disjoint?(%Slot{from: f1, to: t1}, %Slot{from: f2, to: t2}),
    do: DateTime.compare(t1, f2) == :lt or DateTime.compare(f1, t2) == :gt

  @spec join(slots :: Enum.t()) :: Slot.t()
  @doc """
  Joins slots to the maximal covered timeslice.

  ### Example

      iex> Tempus.Slot.join([Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|)])
      #Slot<[from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]]>
  """
  def join(slots) do
    Enum.reduce(slots, fn slot, acc ->
      from =
        if DateTime.compare(slot.from, acc.from) == :lt,
          do: slot.from,
          else: acc.from

      to =
        if DateTime.compare(slot.to, acc.to) == :gt,
          do: slot.to,
          else: acc.to

      %Slot{from: from, to: to}
    end)
  end

  @spec compare(s1 :: t(), s2 :: t()) :: :lt | :gt | :eq
  @doc """
  Compares two slot structs.

  Returns `:gt` if first slot is later than the second and `:lt` for vice versa.
  If the two slots are equal `:eq` is returned.

  Might be used in `Enum.sort/2`.
  """
  def compare(%Slot{} = s, %Slot{} = s), do: :eq

  def compare(%Slot{from: f1, to: t1}, %Slot{from: f2, to: t2}) do
    with :eq <- DateTime.compare(f1, f2), do: DateTime.compare(t1, t2)
  end

  @spec strict_compare(s1 :: Slot.t(), s2 :: Slot.t()) :: :eq | :lt | :gt | :joint
  @doc """
  Compares two slot structs. The same as `compare/2`, but returns `:joint` if
  the slots are overlapped.
  """
  def strict_compare(%Slot{} = s1, %Slot{} = s2) do
    if disjoint?(s1, s2), do: compare(s1, s2), else: :joint
  end

  @spec wrap(origin(), DateTime.t()) :: Slot.t()
  @doc """
  Wraps the argument into a slot. For `DateTime` it’d be a single microsecond.
  For a `Date`, it would be the whole day, starting at `00:00:00.000000` and
      ending at `23:59:59:999999`.

  ## Examples

      iex> Tempus.Slot.wrap(~D|2020-08-06|)
      #Slot<[from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]]>
  """
  def wrap(moment, origin \\ DateTime.utc_now())

  def wrap(nil, origin), do: wrap(DateTime.utc_now(), origin)
  def wrap(%Slot{} = slot, _), do: slot
  def wrap(%DateTime{} = dt, _), do: %Slot{from: dt, to: dt}

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

  def wrap(other, _),
    do: raise(Tempus.ArgumentError, expected: "Tempus.Slot.origin()", passed: other)

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%Tempus.Slot{from: from, to: to}, opts) do
      concat(["#Slot<", to_doc([from: from, to: to], opts), ">"])
    end
  end
end
