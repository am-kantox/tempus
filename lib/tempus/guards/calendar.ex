defmodule Tempus.Guards.Calendar do
  @moduledoc false

  calendars = Application.compile_env(:tempus, :calendars, [Calendar.ISO])
  anno_domini = Application.compile_env(:tempus, :anno_domini, 1970)

  @doc false
  def anno_domini, do: unquote(anno_domini)

  @doc false
  case calendars -- [Calendar.ISO] do
    [] ->
      defguard is_iso_calendar(_) when true
      defguard is_coming_before_in_era_ms(_dt1, _dt2) when false
      defguard is_equal_in_era_ms(_dt1, _dt2) when false

    _ ->
      default_years_ahead = Application.compile_env(:tempus, :default_years_ahead, 100)

      epoch_year =
        Application.compile_env(:tempus, :epoch_year, %{Calendar.ISO => anno_domini})

      years_ahead =
        Application.compile_env(:tempus, :years_ahead, %{Calendar.ISO => default_years_ahead})

      days_of_era =
        for calendar <- calendars,
            from_year = Map.get(epoch_year, calendar, anno_domini),
            upto_year =
              Date.utc_today(calendar).year + Map.get(years_ahead, calendar, default_years_ahead),
            subtract_days = from_year |> calendar.day_of_era(1, 1) |> elem(0),
            year <- from_year..upto_year,
            month <- 1..calendar.months_in_year(year),
            day <- 1..calendar.days_in_month(year, month),
            reduce: %{} do
          acc ->
            {day_of_era, 1} = calendar.day_of_era(year, month, day)

            put_in(
              acc,
              [
                Access.key(calendar, %{}),
                Access.key(year, %{}),
                Access.key(month, %{}),
                Access.key(day, %{})
              ],
              day_of_era - subtract_days
            )
        end

      @days_of_era Macro.escape(days_of_era)

      defmacro is_iso_calendar(data) do
        quote do
          :erlang.map_get(:calendar, unquote(data)) == Calendar.ISO
        end
      end

      defmacro day_of_era(data) do
        quote do
          :erlang.map_get(
            :erlang.map_get(:day, unquote(data)),
            :erlang.map_get(
              :erlang.map_get(:month, unquote(data)),
              :erlang.map_get(
                :erlang.map_get(:year, unquote(data)),
                :erlang.map_get(:erlang.map_get(:calendar, unquote(data)), unquote(@days_of_era))
              )
            )
          )
        end
      end

      defmacro microseconds_from_era(data) do
        quote do
          :erlang.element(1, :erlang.map_get(:microsecond, unquote(data))) +
            (:erlang.map_get(:second, unquote(data)) -
               :erlang.map_get(:utc_offset, unquote(data)) -
               :erlang.map_get(:std_offset, unquote(data)) +
               :erlang.map_get(:minute, unquote(data)) * 60 +
               :erlang.map_get(:hour, unquote(data)) * 3600 +
               day_of_era(unquote(data)) * 86_400) * 1_000_000
        end
      end

      @doc """
      For non-ISO calendars, checks if the former argument comes before the latter one.

      Allowed in guard tests. Inlined by the compiler.
      """
      defguard is_coming_before_in_era_ms(dt1, dt2)
               when not (is_iso_calendar(dt1) and is_iso_calendar(dt2)) and
                      microseconds_from_era(dt1) < microseconds_from_era(dt2)

      @doc """
      For non-ISO calendars, checks if the former argument equals to the latter one.

      Allowed in guard tests. Inlined by the compiler.
      """
      defguard is_equal_in_era_ms(dt1, dt2)
               when not (is_iso_calendar(dt1) and is_iso_calendar(dt2)) and
                      microseconds_from_era(dt1) === microseconds_from_era(dt2)
  end
end
