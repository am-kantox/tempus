defmodule Tempus.Guards do
  @moduledoc since: "0.9.0"
  @moduledoc "Handy guards to simplify pattern matching slots"

  alias Tempus.Slot

  import Tempus.Guards.Calendar,
    only: [is_coming_before_in_era_ms: 2, is_equal_in_era_ms: 2, is_iso_calendar: 1]

  @dialyzer :no_contracts

  @doc """
  Guard to validate that the term given is actually a `t:Tempus.Slot.origin/0`

  ## Examples

      iex> import Tempus.Guards, only: [is_origin: 1]
      ...> is_origin(Date.utc_today())
      true
      ...> is_origin(nil)
      true
      ...> is_origin(:ok)
      false
  """
  @spec is_origin(Slot.origin() | any()) :: boolean()
  defguard is_origin(term)
           when is_nil(term) or
                  (is_map(term) and is_map_key(term, :__struct__) and
                     :erlang.map_get(:__struct__, term) in [Date, DateTime, Time, Slot])

  @doc """
  Guard to validate that the term given is actually a `t:Tempus.Slot.origin/0` _or_
    a function which might be used as a slot locator.

  ## Examples

      iex> import Tempus.Guards, only: [is_locator: 1, is_coming_before: 2]
      ...> is_locator(Date.utc_today())
      true
      ...> is_locator(& Date.utc_today() |> Tempus.Slot.wrap() |> is_coming_before(&1))
      true
      ...> is_locator(true)
      false
  """
  @spec is_locator(Slot.origin() | (Slot.t() -> boolean()) | any()) :: boolean()
  defguard is_locator(origin)
           when is_origin(origin) or is_function(origin, 1)

  @doc """
  Returns `true` if the year is leap, and `false` otherwise.

  ## Examples

      iex> import Tempus.Guards, only: [is_leap: 1]
      ...> is_leap(1970)
      false
      ...> is_leap(2000)
      true

  Allowed in guard tests. Inlined by the compiler.
  """
  defmacro is_leap(year) do
    quote do
      rem(unquote(year), 400) == 0 or
        (rem(unquote(year), 4) == 0 and rem(unquote(year), 100) != 0)
    end
  end

  @anno_domini Tempus.Guards.Calendar.anno_domini()
  @month_justifier %{
    1 => 1,
    2 => 1,
    3 => 0,
    4 => 0,
    5 => 0,
    6 => 0,
    7 => 0,
    8 => 0,
    9 => 0,
    10 => 0,
    11 => 0,
    12 => 0
  }
  @month_adder %{
    1 => 0,
    2 => 31,
    3 => 31 + 28,
    4 => 31 + 28 + 31,
    5 => 31 + 28 + 31 + 30,
    6 => 31 + 28 + 31 + 30 + 31,
    7 => 31 + 28 + 31 + 30 + 31 + 30,
    8 => 31 + 28 + 31 + 30 + 31 + 30 + 31,
    9 => 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31,
    10 => 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
    11 => 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
    12 => 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30
  }
  @leap_days_before_epoch div(@anno_domini, 4) - div(@anno_domini, 100) + div(@anno_domini, 400)
  @microseconds_multiplier Map.new(0..9, &{&1, Integer.pow(10, &1)})

  @doc """
  Macro to convert the `DateTime` struct to epoch. Unlike `DateTime.to_unix/2`,
    it does not support different `Calendar`s but `Calendar.ISO`, and it does not
    support negative epoch times.

  It although supports timezones and can be used in guards.

  ### Examples

      iex> import Tempus.Guards, only: [to_unix: 2]
      ...> dt = DateTime.utc_now()
      ...> to_unix(dt, :second) == DateTime.to_unix(dt, :second)
      true
      ...> to_unix(dt, :millisecond) == DateTime.to_unix(dt, :millisecond)
      true
      ...> to_unix(dt, :microsecond) == DateTime.to_unix(dt, :microsecond)
      true
      ...> to_unix(~U[2024-01-16 14:50:59.787204Z], :microsecond)
      1705416659787204

  Allowed in guard tests. Inlined by the compiler.
  """
  defmacro to_unix(data, unit \\ :microsecond)

  defmacro to_unix(data, :second) do
    quote do
      :erlang.map_get(:year, unquote(data)) >= unquote(@anno_domini) and
        :erlang.map_get(:second, unquote(data)) -
          :erlang.map_get(:utc_offset, unquote(data)) -
          :erlang.map_get(:std_offset, unquote(data)) +
          :erlang.map_get(:minute, unquote(data)) * 60 +
          :erlang.map_get(:hour, unquote(data)) * 3600 +
          (div(
             :erlang.map_get(:year, unquote(data)) -
               :erlang.map_get(
                 :erlang.map_get(:month, unquote(data)),
                 unquote(Macro.escape(@month_justifier))
               ),
             4
           ) -
             div(
               :erlang.map_get(:year, unquote(data)) -
                 :erlang.map_get(
                   :erlang.map_get(:month, unquote(data)),
                   unquote(Macro.escape(@month_justifier))
                 ),
               100
             ) +
             div(
               :erlang.map_get(:year, unquote(data)) -
                 :erlang.map_get(
                   :erlang.map_get(:month, unquote(data)),
                   unquote(Macro.escape(@month_justifier))
                 ),
               400
             ) - unquote(@leap_days_before_epoch)) * 86_400 +
          (:erlang.map_get(:day, unquote(data)) - 1) * 86_400 +
          :erlang.map_get(
            :erlang.map_get(:month, unquote(data)),
            unquote(Macro.escape(@month_adder))
          ) * 86_400 +
          (:erlang.map_get(:year, unquote(data)) - unquote(@anno_domini)) * 31_536_000
    end
  end

  defmacro to_unix(data, precision)
           when is_integer(precision) and precision >= 0 and precision <= 6 do
    quote do
      div(
        :erlang.element(1, :erlang.map_get(:microsecond, unquote(data))),
        :erlang.map_get(6 - unquote(precision), unquote(Macro.escape(@microseconds_multiplier)))
      ) +
        :erlang.map_get(unquote(precision), unquote(Macro.escape(@microseconds_multiplier))) *
          to_unix(unquote(data), :second)
    end
  end

  defmacro to_unix(data, :millisecond) do
    quote do: to_unix(unquote(data), 3)
  end

  defmacro to_unix(data, :microsecond) do
    quote do: to_unix(unquote(data), 6)
  end

  _doc = """
  For ISO calendars, checks if the former argument comes before the latter one.

  Allowed in guard tests. Inlined by the compiler.
  """

  @spec is_coming_before_in_unix_ms(DateTime.t(), DateTime.t()) :: boolean()
  defguardp is_coming_before_in_unix_ms(dt1, dt2)
            when is_iso_calendar(dt1) and is_iso_calendar(dt2) and to_unix(dt1) < to_unix(dt2)

  @doc """
  For any calendar, checks if the former argument comes before the latter one.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_coming_before_in_ms(DateTime.t(), DateTime.t()) :: boolean()
  defguard is_coming_before_in_ms(dt1, dt2)
           when is_coming_before_in_unix_ms(dt1, dt2) or is_coming_before_in_era_ms(dt1, dt2)

  _doc = """
  For ISO calendars, checks if the former argument comes before the latter one.

  Allowed in guard tests. Inlined by the compiler.
  """

  @spec is_equal_in_unix_ms(DateTime.t(), DateTime.t()) :: boolean()
  defguardp is_equal_in_unix_ms(dt1, dt2)
            when is_iso_calendar(dt1) and is_iso_calendar(dt2) and to_unix(dt1) === to_unix(dt2) and
                   to_unix(dt1) != false

  @doc """
  For any calendar, checks if the former argument comes before the latter one.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_equal_in_ms(DateTime.t(), DateTime.t()) :: boolean()
  defguard is_equal_in_ms(dt1, dt2)
           when is_equal_in_unix_ms(dt1, dt2) or is_equal_in_era_ms(dt1, dt2)

  @doc """
  Syntactic sugar for `is_struct(term, Date)`.
  """
  defguard is_date(term) when is_struct(term, Date)

  @doc """
  Syntactic sugar for `is_struct(term, Time)`.
  """
  defguard is_time(term) when is_struct(term, Time)

  @doc """
  Syntactic sugar for `is_struct(term, DateTime)`.
  """
  defguard is_datetime(term) when is_struct(term, DateTime)

  @doc """
  Syntactic sugar for `is_struct(term, Tempus.Slot)`.
  """
  defguard is_slot(term) when is_struct(term, Tempus.Slot)

  defguardp is_like_date(d)
            when is_map(d) and is_map_key(d, :calendar) and is_map_key(d, :year) and
                   is_map_key(d, :month) and is_map_key(d, :day)

  defguardp is_like_time(t)
            when is_map(t) and is_map_key(t, :calendar) and is_map_key(t, :hour) and
                   is_map_key(t, :minute) and is_map_key(t, :second) and
                   is_map_key(t, :microsecond) and is_tuple(:erlang.map_get(:microsecond, t))

  defguardp is_same_calendar(c1, c2)
            when :erlang.map_get(:calendar, c1) == :erlang.map_get(:calendar, c2)

  # Checks two structs passed as parameters for pointing to the same `Date`.
  #
  #     iex> import Tempus.Guards, only: [is_date_equal: 2]
  #     ...> is_date_equal(~D|2000-01-01|, ~U|2000-01-01T12:00:00Z|)
  #     true
  #     iex> is_date_equal(~U|2000-01-01T12:00:00Z|, ~D|1970-01-01|)
  #     false
  #
  # This guard is not exposed because it’s prone to date-like objects in different timezones

  defguardp is_date_equal(d1, d2)
            when is_like_date(d1) and is_like_date(d2) and
                   is_same_calendar(d1, d2) and
                   :erlang.map_get(:year, d1) == :erlang.map_get(:year, d2) and
                   :erlang.map_get(:month, d1) == :erlang.map_get(:month, d2) and
                   :erlang.map_get(:day, d1) == :erlang.map_get(:day, d2)

  # Checks two structs passed as parameters for pointing to the same `Time`.
  #
  #     iex> import Tempus.Guards, only: [is_time_equal: 2]
  #     ...> is_time_equal(~U|1970-01-01T12:00:00Z|, ~U|2000-01-01T12:00:00Z|)
  #     true
  #     iex> is_date_equal(~U|2000-01-01T12:00:00Z|, ~T|23:59:59|)
  #     false
  #
  # This guard is not exposed because it’s prone to time-like objects in different timezones

  defguardp is_time_equal(t1, t2)
            when is_like_time(t1) and is_like_time(t2) and
                   is_same_calendar(t1, t2) and
                   :erlang.map_get(:hour, t1) == :erlang.map_get(:hour, t2) and
                   :erlang.map_get(:minute, t1) == :erlang.map_get(:minute, t2) and
                   :erlang.map_get(:second, t1) == :erlang.map_get(:second, t2) and
                   elem(:erlang.map_get(:microsecond, t1), 0) ==
                     elem(:erlang.map_get(:microsecond, t2), 0)

  defguardp is_timezone_equal(dt1, dt2)
            when :erlang.map_get(:time_zone, dt1) == :erlang.map_get(:time_zone, dt2)

  @doc """
  Guard to validate if two `DateTime` instances passed as parameters for pointing to the same `DateTime`.
  If arguments have different timezones, returns a meaningful value for positive unix epoch only.

  ### Examples

      iex> import Tempus.Guards, only: [is_datetime_equal: 2]
      ...> is_datetime_equal(~U|2020-01-01T12:00:00Z|,
      ...>     DateTime.shift_zone!(~U|2020-01-01T12:00:00Z|, "Australia/Sydney"))
      true
      iex> is_datetime_equal(~U|2000-01-01T12:00:00Z|, ~U|2000-01-01T23:59:59Z|)
      false
  """
  defguard is_datetime_equal(dt1, dt2)
           when is_datetime(dt1) and is_datetime(dt2) and
                  ((not is_timezone_equal(dt1, dt2) and is_equal_in_ms(dt1, dt2)) or
                     (is_timezone_equal(dt1, dt2) and is_date_equal(dt1, dt2) and
                        is_time_equal(dt1, dt2)))

  defguardp is_microsecond_coming_before(m1, m2) when elem(m1, 0) < elem(m2, 0)

  # This guard is not exposed because it’s prone to date-like objects in different timezones
  defguardp is_date_coming_before(d1, d2)
            when is_like_date(d1) and is_like_date(d2) and is_same_calendar(d1, d2) and
                   (:erlang.map_get(:year, d1) < :erlang.map_get(:year, d2) or
                      (:erlang.map_get(:year, d1) == :erlang.map_get(:year, d2) and
                         :erlang.map_get(:month, d1) < :erlang.map_get(:month, d2)) or
                      (:erlang.map_get(:year, d1) == :erlang.map_get(:year, d2) and
                         :erlang.map_get(:month, d1) == :erlang.map_get(:month, d2) and
                         :erlang.map_get(:day, d1) < :erlang.map_get(:day, d2)))

  # This guard is not exposed because it’s prone to time-like objects in different timezones
  defguardp is_time_coming_before(t1, t2)
            when is_like_time(t1) and is_like_time(t2) and is_same_calendar(t1, t2) and
                   (:erlang.map_get(:hour, t1) < :erlang.map_get(:hour, t2) or
                      (:erlang.map_get(:hour, t1) == :erlang.map_get(:hour, t2) and
                         :erlang.map_get(:minute, t1) < :erlang.map_get(:minute, t2)) or
                      (:erlang.map_get(:hour, t1) == :erlang.map_get(:hour, t2) and
                         :erlang.map_get(:minute, t1) == :erlang.map_get(:minute, t2) and
                         :erlang.map_get(:second, t1) < :erlang.map_get(:second, t2)) or
                      (:erlang.map_get(:hour, t1) == :erlang.map_get(:hour, t2) and
                         :erlang.map_get(:minute, t1) == :erlang.map_get(:minute, t2) and
                         :erlang.map_get(:second, t1) == :erlang.map_get(:second, t2) and
                         is_microsecond_coming_before(
                           :erlang.map_get(:microsecond, t1),
                           :erlang.map_get(:microsecond, t2)
                         )))

  @doc """
  Guard to validate if two `DateTime` instances passed as parameters if the former one
    is less (comes earlier) than the latter one.

  If arguments have different timezones, returns a meaningful value for positive unix epoch only.

  If arguments are not datetimes, or timezones differ and values are before epoch, returns `false`.

  ### Examples

      iex> import Tempus.Guards, only: [is_datetime_coming_before: 2]
      ...> is_datetime_coming_before(~U|2020-01-01T12:00:00Z|,
      ...>     DateTime.shift_zone!(~U|2020-01-01T12:00:00Z|, "Australia/Sydney"))
      false
      iex> is_datetime_coming_before(~U|2000-01-01T12:00:00Z|, ~U|2000-01-01T23:59:59Z|)
      true
  """
  defguard is_datetime_coming_before(dt1, dt2)
           when is_datetime(dt1) and is_datetime(dt2) and
                  ((not is_timezone_equal(dt1, dt2) and is_coming_before_in_ms(dt1, dt2)) or
                     (is_timezone_equal(dt1, dt2) and
                        (is_date_coming_before(dt1, dt2) or
                           (is_date_equal(dt1, dt2) and is_time_coming_before(dt1, dt2)))))

  @doc """
  Guard to validate if two `Tempus.Slot` instances passed as parameters if the former one
    is less (comes earlier) than the latter one.

  If arguments have different timezones, returns a meaningful value for positive unix epoch only.

  If arguments are not slots, or timezones differ and values are before epoch, returns `false`.

  ### Examples

      iex> import Tempus.Guards, only: [is_slot_coming_before: 2]
      ...> slot = Tempus.Slot.wrap(~U|2020-01-01T12:00:00Z|)
      ...> is_slot_coming_before(slot, Tempus.Slot.shift_tz(slot, "Australia/Sydney"))
      false
      ...> is_slot_coming_before(slot, Tempus.Slot.wrap(~U|2020-01-01T23:59:59Z|))
      true
  """
  defguard is_slot_coming_before(s1, s2)
           when is_datetime_coming_before(:erlang.map_get(:to, s1), :erlang.map_get(:from, s2))

  defguardp is_datetime_between(dt, dt1, dt2)
            when (is_nil(dt1) and is_datetime(dt2) and not is_datetime_coming_before(dt2, dt)) or
                   (is_nil(dt2) and is_datetime(dt1) and not is_datetime_coming_before(dt, dt1)) or
                   (is_datetime(dt1) and is_datetime(dt2) and
                      not is_datetime_coming_before(dt, dt1) and
                      not is_datetime_coming_before(dt2, dt))

  @doc """
  Guard to validate if the `DateTime` instance passed as a first argument is covered by the slot.

  ### Examples

      iex> import Tempus.Guards, only: [is_datetime_covered: 2]
      ...> slot = Tempus.slot!(~U|2020-01-01T12:00:00Z|, ~U|2020-01-01T23:59:59Z|)
      ...> is_datetime_covered(~U|2020-01-01T18:00:00Z|, slot)
      true
      ...> is_datetime_covered(~U|2020-01-01T06:00:00Z|, slot)
      false
  """
  defguard is_datetime_covered(dt, s)
           when is_slot(s) and
                  is_datetime_between(dt, :erlang.map_get(:from, s), :erlang.map_get(:to, s))

  @doc """
  Guard to validate if the slot instance passed as a first argument is covered by the slot passed last.

  ### Examples

      iex> import Tempus.Guards, only: [is_slot_covered: 2]
      ...> slot = Tempus.slot!(~U|2020-01-01T12:00:00Z|, ~U|2020-01-01T23:59:59Z|)
      ...> covering = Tempus.Slot.wrap(~D|2020-01-01|)
      ...> joint = Tempus.slot!(~U|2020-01-01T06:00:00Z|, ~U|2020-01-01T18:00:00Z|)
      ...> is_slot_covered(slot, covering)
      true
      iex> is_slot_covered(slot, joint)
      false
  """
  defguard is_slot_covered(s1, s2)
           when is_slot(s1) and is_slot(s2) and
                  is_datetime_covered(:erlang.map_get(:from, s1), s2) and
                  is_datetime_covered(:erlang.map_get(:to, s1), s2)

  defguardp is_slot_from_equal(s, dt)
            when is_slot(s) and is_datetime_equal(dt, :erlang.map_get(:from, s))

  defguardp is_slot_to_equal(s, dt)
            when is_slot(s) and is_datetime_equal(dt, :erlang.map_get(:to, s))

  @doc """
  Guard to validate whether the slot is `nil` (has neither end set.)

  ### Examples

      iex> import Tempus.Guards, only: [is_slot_nil: 1]
      iex> is_slot_nil(Tempus.Slot.id())
      true
      iex> is_slot_nil(Tempus.Slot.wrap(Date.utc_today()))
      false
      iex> is_slot_nil(:ok)
      false
  """
  @spec is_slot_nil(Slot.t()) :: boolean()
  defguard is_slot_nil(s)
           when is_slot(s) and is_nil(:erlang.map_get(:from, s)) and
                  is_nil(:erlang.map_get(:to, s))

  @doc """
  Guard to validate whether the slot is open (has either end not set)

  Please note, that the slot having both ends set to `nil` is considered
    a special case and is not reported as _open_.

  ### Examples

      iex> import Tempus.Guards, only: [is_slot_open: 1]
      iex> is_slot_open(%Tempus.Slot{from: nil, to: DateTime.utc_now()})
      true
      iex> is_slot_open(Tempus.Slot.wrap(Date.utc_today()))
      false
      iex> is_slot_open(:ok)
      false
  """
  @spec is_slot_open(Slot.t()) :: boolean()
  defguard is_slot_open(s)
           when is_slot(s) and not is_slot_nil(s) and
                  (is_nil(:erlang.map_get(:from, s)) or
                     is_nil(:erlang.map_get(:to, s)))

  @doc """
  Guard to validate whether the `t:DateTime.t/0` given as the first argument
    is the border of the slot.

  ### Examples

      iex> import Tempus.Guards, only: [is_slot_border: 2]
      iex> dt = DateTime.utc_now()
      ...> is_slot_border(dt, %Tempus.Slot{from: dt, to: nil})
      true
      iex> is_slot_border(dt, Tempus.Slot.wrap(Date.utc_today()))
      false
  """
  @spec is_slot_border(DateTime.t(), Slot.t()) :: boolean()
  defguard is_slot_border(dt, s)
           when is_slot(s) and is_datetime(dt) and
                  (is_slot_from_equal(s, dt) or is_slot_to_equal(s, dt))

  @doc """
  Guard to validate whether two slots are equal

  ## Examples

      iex> import Tempus.Guards, only: [is_slot_equal: 2]
      ...> import Tempus.Sigils
      ...> s1 = ~I[2023-04-09 00:00:00Z|2023-04-09 23:59:59.999999Z]
      ...> s2 = Tempus.Slot.wrap(~D|2023-04-09|)
      ...> s3 = ~I[2023-04-09 00:00:00Z|2023-04-09 23:59:59Z]
      ...> is_slot_equal(s1, s1)
      true
      iex> is_slot_equal(s1, s2)
      true
      iex> is_slot_equal(s1, s3)
      false
      iex> s_bcn = ~U[2023-06-26T09:30:00Z]
      ...> {:ok, s_ny} = DateTime.shift_zone(s_bcn, "America/New_York")
      ...> is_slot_equal(Tempus.Slot.wrap(s_bcn), Tempus.Slot.wrap(s_ny))
      true

  """
  @spec is_slot_equal(Slot.t(), Slot.t()) :: boolean()
  defguard is_slot_equal(s1, s2)
           when is_slot(s1) and is_slot(s2) and
                  is_datetime_equal(:erlang.map_get(:from, s1), :erlang.map_get(:from, s2)) and
                  is_datetime_equal(:erlang.map_get(:to, s1), :erlang.map_get(:to, s2))

  @doc """
  Guard to validate one slot ovelaps another

  ## Examples

      iex> import Tempus.Guards, only: [is_joint: 2]
      ...> import Tempus.Sigils
      ...> s1 = ~I[2023-04-09 23:00:00Z|2023-04-10 00:59:59Z]
      ...> s2 = Tempus.Slot.wrap(~D|2023-04-10|)
      ...> is_joint(s1, s2)
      true
      iex> s1 = ~I[2023-04-09 23:00:00Z|2023-04-10 00:00:00Z]
      ...> s2 = Tempus.Slot.wrap(~D|2023-04-10|)
      ...> is_joint(s1, s2)
      true
  """
  @spec is_joint(Slot.t(), Slot.t()) :: boolean()
  defguard is_joint(s1, s2)
           when is_slot(s1) and is_slot(s2) and
                  (is_datetime_covered(:erlang.map_get(:from, s1), s2) or
                     is_datetime_covered(:erlang.map_get(:to, s1), s2) or
                     is_datetime_covered(:erlang.map_get(:from, s2), s1) or
                     is_datetime_covered(:erlang.map_get(:to, s2), s1))

  @doc """
  Guard to validate the slot covers the origin passed as the first argument

  ## Examples

      iex> import Tempus.Guards, only: [is_covered: 2]
      ...> import Tempus.Sigils
      ...> {from, to} = {~U[2023-04-10 00:00:00Z], ~U[2023-04-10 00:59:59Z]}
      ...> s = %Tempus.Slot{from: from, to: to}
      ...> is_covered(from, s) and is_covered(to, s)
      true
      iex> s1 = ~I[2023-04-10 00:00:00Z|2023-04-11 00:00:00Z]
      ...> s2 = Tempus.Slot.wrap(~D|2023-04-10|)
      ...> is_covered(s1, s2)
      false
      iex> s1 = ~I[2023-04-10 00:00:00Z|2023-04-11 00:00:00Z]
      ...> s2 = Tempus.Slot.wrap(~D|2023-04-10|)
      ...> is_covered(s1, s2)
      false
      iex> s_bcn = ~U[2023-06-26T09:30:00Z]
      ...> s_ny = ~D|2023-06-26| |> Tempus.Slot.wrap() |> Tempus.Slot.shift_tz("America/New_York")
      ...> is_covered(s_bcn, s_ny)
      true
      iex> s_bcn = ~D[2023-06-26]
      ...> s_ny = Tempus.slot!(~D|2023-06-26|, ~D|2023-06-27|) |> Tempus.Slot.shift_tz("America/New_York")
      ...> is_covered(s_bcn, s_ny)
      false
  """
  @spec is_covered(Slot.t() | DateTime.t(), Slot.t()) :: boolean()
  defguard is_covered(o, s)
           when is_slot(s) and
                  ((is_slot(o) and is_slot_covered(o, s)) or
                     (is_datetime(o) and is_datetime_covered(o, s)))

  @doc """
  Guard to compare two instances of `t:Tempus.Slot.origin/0`

  ## Examples

      iex> import Tempus.Guards, only: [is_coming_before: 2]
      ...> is_coming_before(~D[2023-04-10], ~U[2023-04-11T00:00:00.000000Z])
      true
      iex> is_coming_before(~D[2023-04-10], ~D[2023-04-10])
      false
      iex> s_bcn = ~U[2023-06-26T09:30:00Z]
      ...> s_ny = ~D|2024-06-26| |> Tempus.Slot.wrap() |> Tempus.Slot.shift_tz("America/New_York")
      ...> is_coming_before(s_bcn, s_ny)
      true
  """
  @spec is_coming_before(Date.t() | DateTime.t() | Slot.t(), Date.t() | DateTime.t() | Slot.t()) ::
          boolean()
  defguard is_coming_before(o1, o2)
           when (is_datetime(o1) and is_slot(o2) and
                   is_datetime_coming_before(o1, :erlang.map_get(:from, o2))) or
                  (is_slot(o1) and is_datetime(o2) and
                     is_datetime_coming_before(:erlang.map_get(:to, o1), o2)) or
                  (is_date(o1) and is_date(o2) and is_date_coming_before(o1, o2)) or
                  (is_datetime(o1) and is_date(o2) and is_date_coming_before(o1, o2)) or
                  (is_date(o1) and is_datetime(o2) and is_date_coming_before(o1, o2)) or
                  (is_datetime(o1) and is_datetime(o2) and is_datetime_coming_before(o1, o2)) or
                  (is_slot(o1) and is_slot(o2) and is_slot_coming_before(o1, o2))

  @spec joint_in_delta?(
          Slot.t(),
          Slot.t(),
          non_neg_integer() | [{System.time_unit(), non_neg_integer()}]
        ) :: boolean()
  @doc """
  Helper to validate one slot overlaps another in delta. Unlike guards,
    this function does not expect arguments in the correct order, and would return
    `true` if the slots overlap even if `s2` comes _before_ `s1`.

  ## Examples

      iex> import Tempus.Guards, only: [joint_in_delta?: 3]
      ...> import Tempus.Sigils
      ...> s1 = ~I[2023-04-09 23:00:00Z|2023-04-09 23:59:59Z]
      ...> joint_in_delta?(s1, s1, 1)
      true
      iex> s2 = Tempus.Slot.wrap(~D|2023-04-10|)
      ...> joint_in_delta?(s1, s2, 1)
      true
      iex> joint_in_delta?(s2, s1, 1)
      true
      iex> joint_in_delta?(s1, s2, microsecond: 500)
      false
  """
  def joint_in_delta?(s1, s2, _delta) when is_joint(s1, s2), do: true

  def joint_in_delta?(s1, s2, delta) when is_coming_before(s2, s1) do
    joint_in_delta?(s2, s1, delta)
  end

  def joint_in_delta?(s1, s2, [{unit, delta}])
      when is_coming_before(s1, s2),
      do: abs(DateTime.diff(s1.to, s2.from, unit)) <= delta

  def joint_in_delta?(s1, s2, delta_seconds), do: joint_in_delta?(s1, s2, second: delta_seconds)
end
