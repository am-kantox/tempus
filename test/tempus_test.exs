defmodule Tempus.Test do
  use ExUnit.Case, async: true
  doctest Tempus
  doctest Tempus.Slot
  doctest Tempus.Slots

  alias Tempus.Slots
  # alias Kantox.Commons.CurrencyPair, as: Pair

  test "consuming stream" do
    holidays = [~D|2020-08-06|, ~D|2020-08-13|]
    weekends = Stream.map([~D|2020-08-08|, ~D|2020-08-20|, ~D|2020-08-22|], & &1)
    schedule = holidays |> Enum.into(%Slots{}) |> Slots.merge(weekends)

    assert Slots.size(schedule) == 4
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

  # test "free?/3" do
  #   close = fn
  #     true -> DateTime.from_naive!(~N|2018-01-04 21:00:00|, "America/New_York")
  #     false -> DateTime.from_naive!(~N|2018-01-05 21:00:00|, "America/New_York")
  #   end

  #   open = DateTime.from_naive!(~N|2018-01-08 08:59:59|, "Australia/Sydney")

  #   assert Tempus.free?(Pair.new!("USDEUR"), DateTime.add(close.(false).from, -1, :minute))
  #   refute Tempus.free?(Pair.new!("USDEUR"), DateTime.add(close.(false).from, 1, :minute))

  #   assert Tempus.free?(Pair.new!("USDEUR"), DateTime.add(open, 1, :minute))
  #   refute Tempus.free?(Pair.new!("USDEUR"), DateTime.add(open, -1, :minute))

  #   assert Tempus.free?(Pair.new!("USDILS"), DateTime.add(close.(true).from, -1, :minute))
  #   refute Tempus.free?(Pair.new!("USDEUR"), DateTime.add(close.(true).from, 1, :minute))

  #   assert Tempus.free?(Pair.new!("USDILS"), DateTime.add(open, 1, :minute))
  #   refute Tempus.free?(Pair.new!("USDEUR"), DateTime.add(open, -1, :minute))
  # end

  test "slice/4" do
    slots =
      Enum.into(
        [~D|2020-08-06|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|, ~D|2020-08-14|],
        %Slots{}
      )

    from = ~U[2020-08-08 12:00:00.000000Z]
    to = ~U[2020-08-12 12:00:00.000000Z]

    assert Tempus.slice(slots, from, to, :reluctant) == Enum.into([~D|2020-08-10|], %Slots{})

    assert Tempus.slice(slots, from, to, :greedy) ==
             Enum.into([~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|], %Slots{})

    assert Tempus.slice(slots, to, nil, :reluctant) == Enum.into([~D|2020-08-14|], %Slots{})

    assert Tempus.slice(slots, to, nil, :greedy) ==
             Enum.into([~D|2020-08-12|, ~D|2020-08-14|], %Slots{})

    assert Tempus.slice(slots, nil, from, :reluctant) == Enum.into([~D|2020-08-06|], %Slots{})

    assert Tempus.slice(slots, nil, from, :greedy) ==
             Enum.into([~D|2020-08-06|, ~D|2020-08-08|], %Slots{})

    assert Tempus.slice(slots, nil, nil, :greedy) == slots
  end

  test "merge/2" do
    micros_in_three_days = 259_200_000_000

    slots =
      Enum.into(
        [~D|2020-08-06|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|, ~D|2020-08-14|],
        %Slots{}
      )

    stream =
      Stream.iterate(
        Tempus.Slot.wrap(~D|2020-08-06|),
        fn acc ->
          acc
          |> Tempus.Slot.shift(from: micros_in_three_days, to: micros_in_three_days)
          |> Tempus.Slot.shift_tz()
        end
      )

    assert Tempus.Slots.merge(slots, stream) == %Slots{
             slots: [
               %Tempus.Slot{
                 from: ~U[2020-08-06 00:00:00.000000Z],
                 to: ~U[2020-08-06 23:59:59.999999Z]
               },
               %Tempus.Slot{
                 from: ~U[2020-08-08 00:00:00.000000Z],
                 to: ~U[2020-08-10 23:59:59.999999Z]
               },
               %Tempus.Slot{
                 from: ~U[2020-08-12 00:00:00.000000Z],
                 to: ~U[2020-08-12 23:59:59.999999Z]
               },
               %Tempus.Slot{
                 from: ~U[2020-08-14 00:00:00.000000Z],
                 to: ~U[2020-08-15 23:59:59.999999Z]
               }
             ]
           }
  end

  test "add/4" do
    slots =
      [
        Tempus.Slot.wrap(~D|2020-08-07|),
        Tempus.Slot.shift(
          %Tempus.Slot{
            from: ~U|2020-08-08 01:01:00Z|,
            to: ~U|2020-08-08 01:02:00Z|
          },
          to: -1,
          unit: :microsecond
        ),
        Tempus.Slot.shift(
          %Tempus.Slot{
            from: ~U|2020-08-08 01:03:00Z|,
            to: ~U|2020-08-08 01:04:00Z|
          },
          to: -1,
          unit: :microsecond
        )
      ]
      |> Enum.into(%Tempus.Slots{})

    assert ~U|2020-08-08 01:02:00Z| ==
             Tempus.add(slots, ~U|2020-08-08 01:01:30Z|, 0, :second)

    origin = ~U|2020-08-08 01:02:30Z|

    assert ~U|2020-08-08 01:02:30Z| == Tempus.add(slots, origin, 0, :second)
    assert ~U|2020-08-08 01:02:40Z| == Tempus.add(slots, origin, 10, :second)

    assert ~U|2020-08-08 01:04:10.000000Z| ==
             Tempus.add(slots, origin, 40_000_000, :microsecond)

    assert ~U|2020-08-08 01:02:20Z| == Tempus.add(slots, origin, -10, :second)

    assert ~U|2020-08-08 01:00:50Z| ==
             Tempus.add(slots, origin, -40, :second)

    assert ~U|2020-08-08 01:00:50.000000Z| ==
             Tempus.add(slots, origin, -40_000_000, :microsecond)

    assert DateTime.add(origin, 1_000) ==
             Tempus.add(%Slots{}, origin, 1_000, :second)

    assert DateTime.add(origin, -1_000) ==
             Tempus.add(%Slots{}, origin, -1_000, :second)
  end
end
