defmodule Tempus.Sigils.Test do
  use ExUnit.Case, async: false
  doctest Tempus.Sigils

  alias Tempus.Slot
  import Tempus.Sigils
  import ExUnit.CaptureIO

  test "sigils" do
    assert ~I[2021-03-30]d == %Slot{
             from: ~U[2021-03-30 00:00:00.000000Z],
             to: ~U[2021-03-30 23:59:59.999999Z]
           }

    assert ~I[07:00:00]t == %Slot{
             from: %DateTime{
               DateTime.utc_now()
               | hour: 7,
                 minute: 0,
                 second: 0,
                 microsecond: {0, 0}
             },
             to: %DateTime{
               DateTime.utc_now()
               | hour: 7,
                 minute: 0,
                 second: 0,
                 microsecond: {0, 0}
             }
           }
  end

  test "fancy inspect" do
    assert capture_io(fn ->
             # credo:disable-for-next-line
             IO.inspect(~I[2021-03-30]d, custom_options: [fancy: true])
           end) == "#Slot<2021-03-30T00:00:00.000000Z → 2021-03-30T23:59:59.999999Z>\n"

    assert capture_io(fn ->
             # credo:disable-for-next-line
             IO.inspect(~I[2021-03-30]d, custom_options: [fancy: :emoji])
           end) == "⌚<2021-03-30T00:00:00.000000Z → 2021-03-30T23:59:59.999999Z>\n"
  end
end
