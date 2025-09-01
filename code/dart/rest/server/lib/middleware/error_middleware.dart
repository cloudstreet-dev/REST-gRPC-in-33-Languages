import 'dart:convert';
import 'package:shelf/shelf.dart';
import '../services/task_service.dart';

Middleware errorMiddleware() {
  return (Handler innerHandler) {
    return (Request request) async {
      try {
        return await innerHandler(request);
      } on NotFoundException catch (e) {
        return Response.notFound(
          jsonEncode({
            'error': {
              'code': 'NOT_FOUND',
              'message': e.toString(),
            }
          }),
          headers: {'Content-Type': 'application/json'},
        );
      } on ValidationException catch (e) {
        return Response.badRequest(
          body: jsonEncode({
            'error': {
              'code': 'VALIDATION_ERROR',
              'message': e.toString(),
            }
          }),
          headers: {'Content-Type': 'application/json'},
        );
      } on FormatException catch (e) {
        return Response.badRequest(
          body: jsonEncode({
            'error': {
              'code': 'FORMAT_ERROR',
              'message': 'Invalid request format: ${e.message}',
            }
          }),
          headers: {'Content-Type': 'application/json'},
        );
      } catch (e) {
        print('Unhandled error: $e');
        return Response.internalServerError(
          body: jsonEncode({
            'error': {
              'code': 'INTERNAL_ERROR',
              'message': 'An internal error occurred',
            }
          }),
          headers: {'Content-Type': 'application/json'},
        );
      }
    };
  };
}