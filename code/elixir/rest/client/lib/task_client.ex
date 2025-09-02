defmodule TaskClient do
  @moduledoc """
  HTTP client for Task Management REST API
  """

  @base_url "http://localhost:8080/api"
  @headers [{"Content-Type", "application/json"}]

  def list_tasks(filters \\ %{}) do
    query = build_query_string(filters)
    url = "#{@base_url}/tasks#{query}"
    
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, Jason.decode!(body)}
      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed with status #{status}"}
      {:error, error} ->
        {:error, error}
    end
  end

  def get_task(id) do
    url = "#{@base_url}/tasks/#{id}"
    
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, Jason.decode!(body)}
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        {:error, :not_found}
      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed with status #{status}"}
      {:error, error} ->
        {:error, error}
    end
  end

  def create_task(attrs) do
    url = "#{@base_url}/tasks"
    body = Jason.encode!(attrs)
    
    case HTTPoison.post(url, body, @headers) do
      {:ok, %HTTPoison.Response{status_code: 201, body: body}} ->
        {:ok, Jason.decode!(body)}
      {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
        {:error, "Failed with status #{status}: #{body}"}
      {:error, error} ->
        {:error, error}
    end
  end

  def update_task(id, attrs) do
    url = "#{@base_url}/tasks/#{id}"
    body = Jason.encode!(attrs)
    
    case HTTPoison.put(url, body, @headers) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, Jason.decode!(body)}
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        {:error, :not_found}
      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed with status #{status}"}
      {:error, error} ->
        {:error, error}
    end
  end

  def update_task_status(id, status) do
    url = "#{@base_url}/tasks/#{id}/status"
    body = Jason.encode!(%{status: status})
    
    case HTTPoison.patch(url, body, @headers) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, Jason.decode!(body)}
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        {:error, :not_found}
      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed with status #{status}"}
      {:error, error} ->
        {:error, error}
    end
  end

  def delete_task(id) do
    url = "#{@base_url}/tasks/#{id}"
    
    case HTTPoison.delete(url) do
      {:ok, %HTTPoison.Response{status_code: 204}} ->
        :ok
      {:ok, %HTTPoison.Response{status_code: 404}} ->
        {:error, :not_found}
      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed with status #{status}"}
      {:error, error} ->
        {:error, error}
    end
  end

  defp build_query_string(params) when params == %{}, do: ""
  defp build_query_string(params) do
    query = params
    |> Enum.filter(fn {_k, v} -> v != nil end)
    |> Enum.map(fn {k, v} -> "#{k}=#{URI.encode_www_form(to_string(v))}" end)
    |> Enum.join("&")
    
    if query == "", do: "", else: "?#{query}"
  end
end