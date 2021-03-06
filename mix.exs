defmodule Harnais.Error.Mixfile do
  use Mix.Project

  @version "0.3.0"

  def project do
    [
      app: :harnais_error,
      version: @version,
      description: description(),
      package: package(),
      source_url: "https://github.com/ianrumford/harnais_error",
      homepage_url: "https://github.com/ianrumford/harnais_error",
      docs: [extras: ["./README.md", "./CHANGELOG.md"]],
      elixir: "~> 1.6",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:plymio_codi, ">= 0.3.0"},
      {:harnais, ">= 1.0.0"},
      {:ex_doc, "~> 0.18.3", only: :dev}
    ]
  end

  defp package do
    [
      maintainers: ["Ian Rumford"],
      files: ["lib", "mix.exs", "README*", "LICENSE*", "CHANGELOG*"],
      licenses: ["MIT"],
      links: %{github: "https://github.com/ianrumford/harnais_error"}
    ]
  end

  defp description do
    """
    harnais_error: The Exception for the Harnais Package Family
    """
  end
end
