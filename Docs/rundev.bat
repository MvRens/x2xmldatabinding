@Echo Off
start "" "http://localhost:8000/"
sphinx-autobuild . .\_build\html -N