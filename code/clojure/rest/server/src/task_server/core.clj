(ns task-server.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :as json]
            [ring.middleware.params :as params]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.cors :as cors]
            [compojure.core :refer [defroutes GET POST PUT PATCH DELETE OPTIONS routes context]]
            [compojure.route :as route]
            [task-server.handlers :as handlers]
            [taoensso.timbre :as log])
  (:gen-class))

(defroutes api-routes
  (context "/api" []
    (GET "/tasks" request (handlers/list-tasks-handler request))
    (GET "/tasks/:id" request (handlers/get-task-handler request))
    (POST "/tasks" request (handlers/create-task-handler request))
    (PUT "/tasks/:id" request (handlers/update-task-handler request))
    (PATCH "/tasks/:id/status" request (handlers/update-status-handler request))
    (DELETE "/tasks/:id" request (handlers/delete-task-handler request))))

(defroutes app-routes
  (GET "/health" request (handlers/health-handler request))
  (OPTIONS "/*" [] {:status 204 :headers {}})
  api-routes
  (route/not-found {:error "Not found"}))

(def app
  (-> app-routes
      (cors/wrap-cors :access-control-allow-origin [#".*"]
                      :access-control-allow-methods [:get :post :put :patch :delete :options]
                      :access-control-allow-headers ["Content-Type"])
      (json/wrap-json-body {:keywords? true})
      (json/wrap-json-response)
      keyword-params/wrap-keyword-params
      params/wrap-params))

(defn print-banner []
  (println "╔════════════════════════════════════════════════╗")
  (println "║      Clojure Task Management REST API          ║")
  (println "║         Built with Ring & Compojure            ║")
  (println "╚════════════════════════════════════════════════╝")
  (println))

(defn -main
  "Start the server"
  [& args]
  (print-banner)
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (log/info (str "[INFO] Clojure Task REST Server starting on port " port))
    (log/info (str "[INFO] Visit http://localhost:" port "/api/tasks\n"))
    (println "Available endpoints:")
    (println "  GET    /api/tasks          - List all tasks")
    (println "  GET    /api/tasks/{id}     - Get a specific task")
    (println "  POST   /api/tasks          - Create a new task")
    (println "  PUT    /api/tasks/{id}     - Update a task")
    (println "  PATCH  /api/tasks/{id}/status - Update task status")
    (println "  DELETE /api/tasks/{id}     - Delete a task")
    (println "  GET    /health             - Health check\n")
    (println "Sample requests:")
    (println (str "  curl http://localhost:" port "/api/tasks"))
    (println (str "  curl -X POST http://localhost:" port "/api/tasks \\"))
    (println "    -H \"Content-Type: application/json\" \\")
    (println "    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'\n")
    (println "[INFO] Press Ctrl+C to stop the server\n")
    (jetty/run-jetty app {:port port :join? true})))