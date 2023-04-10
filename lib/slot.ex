defmodule Tempus.Slot do
  @moduledoc """
  Declares a timeslot and exports functions to check whether the given date
    and/or datetime is covered by this slot or not.

  This module probably should not be called directly.
  """
  alias __MODULE__

  @typedoc "A timeslot to be used in `Tempus`"
  @type t :: %__MODULE__{
          from: nil | DateTime.t(),
          to: nil | DateTime.t()
        }

  @typedoc "The origin used in comparisons and calculations"
  @type origin :: Slot.t() | Date.t() | DateTime.t() | Time.t() | nil

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
  def valid?(%Slot{from: nil, to: %DateTime{}}), do: true
  def valid?(%Slot{from: %DateTime{}, to: nil}), do: true

  def valid?(%Slot{from: %DateTime{} = from, to: %DateTime{} = to}),
    do: DateTime.compare(from, to) != :gt

  def valid?(_), do: false

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

  def cover?(%Slot{from: nil, to: nil}, _, _),
    do: false

  def cover?(%Slot{from: nil, to: %DateTime{} = to}, %DateTime{} = dt, true),
    do: DateTime.compare(to, dt) == :gt

  def cover?(%Slot{from: %DateTime{} = from, to: nil}, %DateTime{} = dt, true),
    do: DateTime.compare(from, dt) == :lt

  def cover?(%Slot{from: %DateTime{} = from, to: %DateTime{} = to}, %DateTime{} = dt, true),
    do: DateTime.compare(from, dt) == :lt and DateTime.compare(to, dt) == :gt

  def cover?(%Slot{from: from, to: to} = slot, %DateTime{} = dt, false),
    do:
      cover?(slot, dt, true) or
        (not is_nil(from) and DateTime.compare(from, dt) == :eq) or
        (not is_nil(to) and DateTime.compare(to, dt) == :eq)

  def cover?(%Slot{} = slot, %Date{} = dt, strict?),
    do: cover?(slot, wrap(dt, slot.from || slot.to), strict?)

  def cover?(%Slot{} = slot, %Time{} = dt, strict?),
    do: cover?(slot, wrap(dt, slot.from || slot.to), strict?)

  def cover?(%Slot{from: nil, to: %DateTime{}}, %Slot{from: nil, to: %DateTime{}}, true),
    do: false

  def cover?(
        %Slot{from: nil, to: %DateTime{} = s_to},
        %Slot{from: nil, to: %DateTime{} = dt_to},
        false
      ),
      do: DateTime.compare(s_to, dt_to) in [:lt, :eq]

  def cover?(%Slot{from: %DateTime{}, to: nil}, %Slot{from: %DateTime{}, to: nil}, true),
    do: false

  def cover?(
        %Slot{from: %DateTime{} = s_from, to: nil},
        %Slot{from: %DateTime{} = dt_from, to: nil},
        false
      ),
      do: DateTime.compare(s_from, dt_from) in [:gt, :eq]

  def cover?(%Slot{} = slot, %Slot{from: from, to: to}, strict?),
    do: cover?(slot, from, strict?) and cover?(slot, to, strict?)

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
  """
  def disjoint?(s1, s2) do
    [%Slot{} = s1, %Slot{} = s2] = Enum.map([s1, s2], &wrap/1)
    compare(s1, s2) in [:lt, :gt]
  end

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

      iex> Tempus.Slot.intersect([Tempus.Slot.wrap(~D|2020-09-30|),
      ...>   %Tempus.Slot{from: ~U|2020-09-30 23:00:00Z|, to: ~U|2020-10-02 00:00:00Z|}])
      #Slot<[from: ~U[2020-09-30 23:00:00Z], to: ~U[2020-09-30 23:59:59.999999Z]]>
  """
  def intersect(slots) do
    Enum.reduce(slots, fn
      _slot, nil ->
        nil

      slot, acc ->
        slot = wrap(slot)

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

      iex> Tempus.Slot.join([Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|)])
      #Slot<[from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]]>

      iex> Tempus.Slot.join([~D|2020-09-30|, ~D|2020-10-02|])
      #Slot<[from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]]>
  """
  def join([]), do: %Slot{from: nil, to: nil}

  def join([slot | slots]) do
    Enum.reduce(slots, wrap(slot), fn slot, acc ->
      slot = wrap(slot)

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

  @spec join(Slot.t(), Slot.t()) :: Slot.t()
  @doc """
  Joins two slots to the maximal covered timeslice.

  ### Example

      iex> Tempus.Slot.join(Tempus.Slot.wrap(~D|2020-09-30|), Tempus.Slot.wrap(~D|2020-10-02|))
      #Slot<[from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]]>

      iex> Tempus.Slot.join(~D|2020-09-30|, ~D|2020-10-02|)
      #Slot<[from: ~U[2020-09-30 00:00:00.000000Z], to: ~U[2020-10-02 23:59:59.999999Z]]>
  """
  def join(s1, s2), do: join([s1, s2])

  @spec duration(slot :: Slot.t(), unit :: System.time_unit()) :: non_neg_integer() | :infinity
  @doc """
  Calculates the duration of a slot in units given as a second parameter
    (default: `:second`.)

  ### Example

      iex> ~D|2020-09-03| |> Tempus.Slot.wrap() |> Tempus.Slot.duration()
      86400
  """
  def duration(slot, unit \\ :second)
  def duration(%Slot{from: nil, to: %DateTime{}}, _), do: :infinity
  def duration(%Slot{from: %DateTime{}, to: nil}, _), do: :infinity

  def duration(%Slot{from: %DateTime{} = from, to: %DateTime{} = to}, unit),
    do: to |> DateTime.add(1, unit) |> DateTime.diff(from, unit)

  @spec compare(s1 :: t(), s2 :: t(), strict :: boolean()) :: :lt | :gt | :eq | :joint
  @doc """
  Compares two slot structs.

  Returns `:gt` if first slot is strictly later than the second and `:lt` for vice versa.
  **NB** `:eq` is returned not only if slots are equal, but also when they are overlapped.

  Might be used in `Enum.sort/2`.
  """
  def compare(s1, s2, strict \\ false)

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

  @spec strict_compare(s1 :: Slot.t(), s2 :: Slot.t()) :: :eq | :lt | :gt | :joint
  @doc """
  Compares two slot structs. The same as `compare/2`, but returns `:joint` if
  the slots are overlapped.
  """
  def strict_compare(%Slot{} = s1, %Slot{} = s2),
    do: compare(s1, s2, true)

  @spec wrap(origin(), DateTime.t()) :: Slot.t()
  @doc """
  Wraps the argument into a slot. For `DateTime` it’d be a single microsecond.
  For a `Date`, it would be the whole day, starting at `00:00:00.000000` and
      ending at `23:59:59:999999`.

  ## Examples

      iex> Tempus.Slot.wrap(~D|2020-08-06|)
      #Slot<[from: ~U[2020-08-06 00:00:00.000000Z], to: ~U[2020-08-06 23:59:59.999999Z]]>
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

  @doc false
  @spec shift(
          slot :: t(),
          action :: [{:to, integer()} | {:from, integer} | {:unit, System.time_unit()}]
        ) :: Slot.t()
  def shift(%Slot{from: from, to: to}, action \\ []) do
    unit = Keyword.get(action, :unit, :microsecond)
    from = do_shift(from, Keyword.get(action, :from, 0), unit)
    to = do_shift(to, Keyword.get(action, :to, 0), unit)

    %Slot{from: from, to: to}
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
  #⇒ #Slot<[from: ~U[2018-01-06 02:00:00Z], to: ~U[2018-01-07 21:59:59Z]]>
  ```
  """
  def shift_tz(
        %Slot{from: from, to: to},
        tz \\ "Etc/UTC",
        tz_db \\ Calendar.get_time_zone_database()
      ) do
    %Slot{from: DateTime.shift_zone!(from, tz, tz_db), to: DateTime.shift_zone!(to, tz, tz_db)}
  end

  defimpl Inspect do
    @moduledoc false

    import Inspect.Algebra
    @fancy_inspect Application.compile_env(:tempus, :inspect, :sigil)

    def inspect(%Tempus.Slot{from: from, to: to}, %Inspect.Opts{custom_options: [_ | _]} = opts) do
      opts.custom_options
      |> Keyword.get(:fancy, @fancy_inspect)
      |> case do
        truthy when truthy in [:emoji, true] ->
          value = Enum.map_join([from, to], " → ", &DateTime.to_iso8601/1)

          tag =
            case truthy do
              :emoji -> "⌚"
              true -> "#Slot"
            end

          concat([tag, "<", value, ">"])

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
      concat(["#Slot<", to_doc([from: from, to: to], opts), ">"])
    end
  end
end
