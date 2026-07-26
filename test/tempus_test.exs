defmodule Tempus.Test do
  use ExUnit.Case, async: true
  doctest Tempus
  doctest Tempus.Slot
  doctest Tempus.Crontab

  alias Tempus.{Slot, Slots}

  test "Tempus.slot/1" do
    from = ~U[2020-08-08 12:00:00.000000Z]
    to = ~U[2020-08-12 12:00:00.000000Z]

    assert {:ok, %Slot{from: ^from, to: ^to}} = Tempus.slot(from, to)
    assert {:error, :invalid_input} = Tempus.slot(42, to)
    assert {:error, :invalid_input} = Tempus.slot(from, 42)
    assert %Slot{from: ^from, to: ^to} = Tempus.slot!(from, to)
    assert_raise ArgumentError, fn -> Tempus.slot!(from, 42) end
  end

  test "Tempus.Slot.new/1" do
    from = ~U[2020-08-08 12:00:00.000000Z]
    to = ~U[2020-08-12 12:00:00.000000Z]

    assert {:ok, %Slot{from: ^from, to: ^to}} = Tempus.Slot.new(from, to)
    assert {:ok, %Slot{from: nil, to: ^to, from_open: true}} = Tempus.Slot.new(nil, to)
    assert {:ok, %Slot{from: ^from, to: nil, to_open: true}} = Tempus.Slot.new(from, nil)

    assert {:ok, %Slot{from: nil, to: nil, from_open: true, to_open: true}} =
             Tempus.Slot.new(nil, nil)

    assert {:error, :invalid_input} = Tempus.Slot.new(42, to)
    assert {:error, :invalid_input} = Tempus.Slot.new(from, 42)
    assert %Slot{from: ^from, to: ^to} = Tempus.Slot.new!(from, to)
    assert_raise ArgumentError, fn -> Tempus.Slot.new!(from, 42) end
  end

  test "consuming stream" do
    holidays = [~D|2020-08-06|, ~D|2020-08-13|, ~D|2020-08-20|]

    weekends = %Tempus.Slots{
      slots: %Tempus.Slots.Stream{
        slots: Stream.map([~D|2020-08-08|, ~D|2020-08-20|, ~D|2020-08-22|], &Tempus.Slot.wrap/1)
      }
    }

    schedule = holidays |> Enum.into(%Slots{}) |> Slots.merge(weekends) |> Enum.to_list()

    assert length(schedule) == 5
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

  test "slice/4" do
    for kind <- [:list, :stream] do
      empty = Slots.new(kind, [])

      slots =
        Slots.new(kind, [
          ~D|2020-08-06|,
          ~D|2020-08-08|,
          ~D|2020-08-10|,
          ~D|2020-08-12|,
          ~D|2020-08-14|
        ])

      from = ~U[2020-08-08 12:00:00.000000Z]
      to = ~U[2020-08-12 12:00:00.000000Z]

      assert [Tempus.slice(slots, from, to, :reluctant), Enum.into([~D|2020-08-10|], empty)]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)

      assert [
               Tempus.slice(slots, from, to, :greedy),
               Enum.into([~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|], empty)
             ]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)

      assert [Tempus.slice(slots, to, nil, :reluctant), Enum.into([~D|2020-08-14|], empty)]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)

      assert [
               Tempus.slice(slots, to, nil, :greedy),
               Enum.into([~D|2020-08-12|, ~D|2020-08-14|], empty)
             ]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)

      assert [Tempus.slice(slots, nil, from, :reluctant), Enum.into([~D|2020-08-06|], empty)]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)

      assert [
               Tempus.slice(slots, nil, from, :greedy),
               Enum.into([~D|2020-08-06|, ~D|2020-08-08|], empty)
             ]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)

      assert [Tempus.slice(slots, nil, nil, :greedy), slots]
             |> Enum.map(&Enum.to_list/1)
             |> Enum.reduce(&Kernel.==/2)
    end
  end

  test "merge/2" do
    micros_in_three_days = 259_200_000_000

    slots =
      Enum.into(
        [~D|2020-08-06|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|, ~D|2020-08-14|],
        %Slots{}
      )

    stream = %Slots{
      slots:
        Slots.Stream.iterate(
          Tempus.Slot.wrap(~D|2020-08-06|),
          fn acc ->
            acc
            |> Tempus.Slot.shift(from: micros_in_three_days, to: micros_in_three_days)
            |> Tempus.Slot.shift_tz()
          end
        )
    }

    assert Enum.take(Tempus.Slots.merge([slots, stream], join: true), 4) == [
             %Tempus.Slot{
               from: ~U[2020-08-06 00:00:00.000000Z],
               to: ~U[2020-08-07 00:00:00.000000Z],
               from_open: false,
               to_open: true
             },
             %Tempus.Slot{
               from: ~U[2020-08-08 00:00:00.000000Z],
               to: ~U[2020-08-11 00:00:00.000000Z],
               from_open: false,
               to_open: true
             },
             %Tempus.Slot{
               from: ~U[2020-08-12 00:00:00.000000Z],
               to: ~U[2020-08-13 00:00:00.000000Z],
               from_open: false,
               to_open: true
             },
             %Tempus.Slot{
               from: ~U[2020-08-14 00:00:00.000000Z],
               to: ~U[2020-08-16 00:00:00.000000Z],
               from_open: false,
               to_open: true
             }
           ]
  end

  test "add/4" do
    slots =
      [
        Tempus.Slot.wrap(~D|2020-08-07|),
        %Tempus.Slot{
          from: ~U|2020-08-08 01:01:00Z|,
          to: ~U|2020-08-08 01:02:00Z|,
          from_open: false,
          to_open: true
        },
        %Tempus.Slot{
          from: ~U|2020-08-08 01:03:00Z|,
          to: ~U|2020-08-08 01:04:00Z|,
          from_open: false,
          to_open: true
        }
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

  describe "Slot advanced operations" do
    test "gap/1" do
      s1 = %Slot{from: ~U[2020-01-01 00:00:00Z], to: ~U[2020-01-01 12:00:00Z], from_open: false, to_open: true}
      s2 = %Slot{from: ~U[2020-01-01 14:00:00Z], to: ~U[2020-01-01 18:00:00Z], from_open: false, to_open: true}
      assert %Slot{from: ~U[2020-01-01 12:00:00Z], to: ~U[2020-01-01 14:00:00Z], from_open: false, to_open: true} = Slot.gap([s1, s2])
      assert %Slot{from: ~U[2020-01-01 12:00:00Z], to: ~U[2020-01-01 14:00:00Z], from_open: false, to_open: true} = Slot.gap([s2, s1])
      assert %Slot{from: ~U[2020-01-01 12:00:00Z], to: nil, from_open: false, to_open: true} = Slot.gap([%Slot{from: nil, to: ~U[2020-01-01 12:00:00Z]}])
      assert %Slot{from: nil, to: ~U[2020-01-01 14:00:00Z], from_open: true, to_open: true} = Slot.gap([%Slot{from: ~U[2020-01-01 14:00:00Z], to: nil}])
      assert %Slot{from: nil, to: nil, from_open: true, to_open: true} = Slot.gap([])
    end

    test "shift/2 and shift_tz/3" do
      s = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z], from_open: false, to_open: true}
      shifted = Slot.shift(s, by: 1, unit: :hour)
      assert DateTime.compare(shifted.from, ~U[2020-01-01 11:00:00Z]) == :eq
      assert DateTime.compare(shifted.to, ~U[2020-01-01 13:00:00Z]) == :eq

      shifted_day = Slot.shift(s, by: 1, unit: :day)
      assert DateTime.compare(shifted_day.from, ~U[2020-01-02 10:00:00Z]) == :eq

      tz_shifted = Slot.shift_tz(s, "UTC")
      assert DateTime.compare(tz_shifted.from, s.from) == :eq
    end

    test "strict_compare/2" do
      s1 = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z]}
      s2 = %Slot{from: ~U[2020-01-01 11:00:00Z], to: ~U[2020-01-01 13:00:00Z]}
      s3 = %Slot{from: ~U[2020-01-01 13:00:00Z], to: ~U[2020-01-01 14:00:00Z]}
      assert Slot.strict_compare(s1, s2) == :joint
      assert Slot.strict_compare(s1, s3) == :lt
      assert Slot.strict_compare(s3, s1) == :gt
    end

    test "disjoint?/2 and neighbour?/2" do
      s1 = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z], from_open: false, to_open: true}
      s2 = %Slot{from: ~U[2020-01-01 12:00:00Z], to: ~U[2020-01-01 14:00:00Z], from_open: false, to_open: true}
      s3 = %Slot{from: ~U[2020-01-01 13:00:00Z], to: ~U[2020-01-01 15:00:00Z], from_open: false, to_open: true}

      assert Slot.disjoint?(s1, s2)
      assert Slot.disjoint?(s1, s3)
      assert Slot.neighbour?(s1, s2)
      refute Slot.neighbour?(s1, s3)
    end

    test "duration/2" do
      s = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z]}
      assert Slot.duration(s, :hour) == 2
      assert Slot.duration(s, :second) == 7200
      assert Slot.duration(%Slot{from: nil, to: ~U[2020-01-01 12:00:00Z]}) == :infinity
      assert Slot.duration(%Slot{from: ~U[2020-01-01 10:00:00Z], to: nil}) == :infinity
    end

    test "xor/2" do
      outer = Slot.wrap(~D[2023-04-12])
      {:ok, inner} = Slot.new(~U[2023-04-12 12:00:00Z], ~U[2023-04-12 13:00:00Z])
      res = Slot.xor(outer, inner)
      assert length(res) == 2
      assert hd(res).from == ~U[2023-04-12 00:00:00.000000Z]
      assert hd(res).to == ~U[2023-04-12 12:00:00Z]
    end
  end

  describe "Guards edge cases" do
    test "is_slot_equal/2 and is_datetime_covered/2" do
      import Tempus.Guards
      s1 = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z], from_open: false, to_open: true}
      s2 = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z], from_open: false, to_open: true}
      s3 = %Slot{from: ~U[2020-01-01 10:00:00Z], to: ~U[2020-01-01 12:00:00Z], from_open: true, to_open: true}

      assert is_slot_equal(s1, s2)
      refute is_slot_equal(s1, s3)

      dt = ~U[2020-01-01 10:00:00Z]
      assert is_datetime_covered(dt, s1)
      refute is_datetime_covered(dt, s3)
    end
  end

  describe "Sigils boundary parsing" do
    test "sigil_I with bracket and paren delimiters" do
      import Tempus.Sigils
      s1 = ~I|[2023-04-10 10:00:00Z → 2023-04-10 12:00:00Z)|
      assert s1.from_open == false
      assert s1.to_open == true

      s2 = ~I|(2023-04-10 10:00:00Z → 2023-04-10 12:00:00Z]|
      assert s2.from_open == true
      assert s2.to_open == false

      s3 = ~I|(2023-04-10 10:00:00Z → 2023-04-10 12:00:00Z)|
      assert s3.from_open == true
      assert s3.to_open == true
    end
  end
end
