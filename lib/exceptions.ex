defmodule Tempus.ArgumentError do
  @moduledoc """
  Raised at runtime when an argument passed to function is invalid
  """
  defexception [:message, :expected, :passed]

  def exception(opts) do
    expected = Keyword.fetch!(opts, :expected)
    passed = Keyword.fetch!(opts, :passed)
    message = "Invalid argument; expected `#{expected}`, passed: `#{inspect(passed)}`"

    %__MODULE__{message: message, expected: expected, passed: passed}
  end
end
