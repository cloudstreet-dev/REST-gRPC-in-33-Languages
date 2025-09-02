defmodule TaskGrpcServer.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Logger.Server
  run TaskGrpcServer.Service
end