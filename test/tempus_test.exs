defmodule Tempus.Test do
  use ExUnit.Case
  doctest Tempus
  doctest Tempus.Slot
  doctest Tempus.Slots

  alias Tempus.Slots

  test "consuming stream" do
    holidays = [~D|2020-08-06|, ~D|2020-08-13|]
    weekends = Stream.map([~D|2020-08-08|, ~D|2020-08-20|], & &1)
    schedule = holidays |> Enum.into(%Slots{}) |> Slots.merge(weekends)

    assert Slots.size(schedule) == 3
  end

  test "working days" do
    holidays = [~D|2020-08-06|, ~D|2020-08-13|]
    weekends = [~D|2020-08-08|, ~D|2020-08-09|]
    schedule = holidays |> Enum.into(%Slots{}) |> Slots.merge(weekends)

    plus_five_wdays = Tempus.days_ahead(schedule, ~D|2020-08-05|, 5)
    assert Date.from_iso8601!("2020-08-14") == hd(plus_five_wdays)
    plus_three_wdays = Tempus.days_ahead(schedule, ~D|2020-08-05|, 3)
    assert Date.from_iso8601!("2020-08-11") == hd(plus_three_wdays)
    plus_zero_wdays = Tempus.days_ahead(schedule, ~D|2020-08-05|, 0)
    assert Date.from_iso8601!("2020-08-05") == hd(plus_zero_wdays)
    plus_zero_wdays = Tempus.days_ahead(schedule, ~D|2020-08-06|, 0)
    assert Date.from_iso8601!("2020-08-07") == hd(plus_zero_wdays)
  end
end
