import 'package:shelf/shelf.dart';

Middleware corsMiddleware() {
  const corsHeaders = {
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'GET, POST, PUT, PATCH, DELETE, OPTIONS',
    'Access-Control-Allow-Headers': 'Origin, Content-Type, Accept, Authorization',
  };

  Response? handleOptions(Request request) {
    if (request.method == 'OPTIONS') {
      return Response.ok('', headers: corsHeaders);
    }
    return null;
  }

  Response addCorsHeaders(Response response) {
    return response.change(headers: corsHeaders);
  }

  return createMiddleware(
    requestHandler: handleOptions,
    responseHandler: addCorsHeaders,
  );
}