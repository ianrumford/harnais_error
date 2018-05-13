defmodule HarnaisErrorTest do
  use ExUnit.Case
  doctest HarnaisError

  test "greets the world" do
    assert HarnaisError.hello() == :world
  end
end
