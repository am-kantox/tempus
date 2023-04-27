defmodule Tempus.Slots.Options do
  @moduledoc false
  def pop_jid(options) do
    case Keyword.get(options, :join, nil) do
      true -> 1
      false -> nil
      value -> value
    end
  end
end
