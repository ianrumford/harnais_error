defmodule HarnaisErrorStatus1Test do
  use HarnaisErrorHelperTest

  test "new: 100a" do
    assert %HES{} = HES.new!()
  end

  test "add_entry: 100a" do
    status =
      HES.new!()
      |> HES.add_entry!({:ok, :k1})
      |> HES.add_entry!({:ok, :k2})
      |> HES.add_entry!({:error, %BadMapError{term: :e1}})
      |> HES.add_entry!({:ok, :k3})
      |> HES.add_entry!({:error, %BadStructError{term: :e2}})

    assert %HES{} = status
  end

  test "add_result: 100a" do
    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status
  end

  test "add_results: 100a" do
    status =
      HES.new!()
      |> HES.add_results!([{:ok, :k1}, {:ok, :k2}, {:error, :e1}, {:ok, :k3}, {:error, :e2}])

    assert %HES{} = status
  end

  test "get_results: 100a" do
    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    results = status |> HES.export!()

    assert results == [
             {:ok, :k1},
             {:ok, :k2},
             {:error, [[v: :e1]]},
             {:ok, :k3},
             {:error, [[v: :e2]]}
           ]
  end

  test "get_oks: 100a" do
    assert "Harnais.Error.Status has no oks" ==
             HES.new!()
             |> HES.get_oks()
             |> elem(1)
             |> Exception.message()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    results = status |> HES.get_oks!()

    assert results == [:k1, :k2, :k3]
  end

  test "get_errors: 100a" do
    assert "Harnais.Error.Status has no errors" ==
             HES.new!()
             |> HES.get_errors()
             |> elem(1)
             |> Exception.message()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    results = status |> HES.get_errors!()

    assert results |> Enum.all?(&Exception.exception?/1)
  end

  test "size: 100a" do
    assert 0 == HES.new!() |> HES.size()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    assert 5 == status |> HES.size()
  end

  test "empty?: 100a" do
    assert HES.new!() |> HES.empty?()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    refute status |> HES.empty?()
  end

  test "reset: 100a" do
    assert %HES{} = HES.new!() |> HES.reset!()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    status = status |> HES.reset!()

    assert %HES{} = status

    assert 0 == status |> HES.size()
    assert status |> HES.empty?()

    assert %HES{} = status |> HES.reset!()
  end

  test "has_results: 100a" do
    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    assert status |> HES.has_results?()
  end

  test "has_oks: 100a" do
    refute HES.new!() |> HES.has_oks?()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    assert status |> HES.has_oks?()
  end

  test "has_errors: 100a" do
    refute HES.new!() |> HES.has_errors?()

    status =
      HES.new!()
      |> HES.add_result!({:ok, :k1})
      |> HES.add_result!({:ok, :k2})
      |> HES.add_result!({:error, :e1})
      |> HES.add_result!({:ok, :k3})
      |> HES.add_result!({:error, :e2})

    assert %HES{} = status

    assert status |> HES.has_errors?()
  end

  test "add_ok: 100a" do
    status =
      HES.new!()
      |> HES.add_ok!(:k1)
      |> HES.add_ok!(:k2)
      |> HES.add_ok!(:k3)

    assert %HES{} = status

    assert 3 = status |> HES.size()

    results = status |> HES.export!()

    assert results == [
             {:ok, :k1},
             {:ok, :k2},
             {:ok, :k3}
           ]

    assert [:k1, :k2, :k3] == status |> HES.get_oks!()
  end

  test "add_error: 100a" do
    status =
      HES.new!()
      |> HES.add_error!(:e1)
      |> HES.add_error!(:e2)

    assert %HES{} = status

    assert 2 = status |> HES.size()

    results = status |> HES.export!()

    assert results == [
             {:error, [[v: :e1]]},
             {:error, [[v: :e2]]}
           ]
  end

  test "reduce_new: 100a" do
    status =
      HES.new!(
        add_ok: :k1,
        add_result: {:ok, :k2},
        add_error: :e1,
        add_result: {:ok, :k3},
        add_result: {:error, :e2}
      )

    assert %HES{} = status

    assert 5 = status |> HES.size()

    assert status |> HES.has_oks?()
    assert status |> HES.has_errors?()

    results = status |> HES.export!()

    assert results == [
             {:ok, :k1},
             {:ok, :k2},
             {:error, [[v: :e1]]},
             {:ok, :k3},
             {:error, [[v: :e2]]}
           ]
  end

  test "new: 300a" do
    assert %HES{} = HES.new!()

    status =
      HES.new!(
        add_ok: :k1,
        add_ok: :k2,
        add_error: :e1,
        add_ok: :k3,
        add_error: :e2,
        add_result: {:ok, :k4},
        add_result: {:error, :e3},
        add_results: [{:error, :e6}, {:ok, :k8}],
        add_errors: [:e4, :e5],
        add_oks: [:k5, :k6, :k7]
      )

    assert %HES{} = status

    results = status |> HES.export!()

    assert results == [
             {:ok, :k1},
             {:ok, :k2},
             {:error, [[v: :e1]]},
             {:ok, :k3},
             {:error, [[v: :e2]]},
             {:ok, :k4},
             {:error, [[v: :e3]]},
             {:error, [[v: :e6]]},
             {:ok, :k8},
             {:error, [[v: :e4]]},
             {:error, [[v: :e5]]},
             {:ok, :k5},
             {:ok, :k6},
             {:ok, :k7}
           ]
  end
end
