#!/bin/bash
set -e

function printHelp {
    cat <<EOF

Optional parameters:
  --trustedUrlBase XXX        - sets the trusted url of the AFCEns Visualizer instance (default: http://localhost:8080)
  --sandboxUrlBase XXX        - sets the sandbox url of the AFCEns Visualizer instance (default: http://localhost:8081)
  --withProxy                 - use if AFCEns Visualizer instance is behind an http reverse proxy
  --afcensEnforcerUrl XXX   - sets AFCEns Enforcer host (default: http://afcens-enforcer:3100/)
  --elasticsearchHost XXX               - sets elasticsearch host (default: elasticsearch)
  --redisHost XXX             - sets redis host (default: redis)
  --mySqlHost XXX             - sets mysql host (default: mysql)
EOF

    exit 1
}


urlBaseTrusted=http://localhost:8080
urlBaseSandbox=http://localhost:8081
withProxy=false
afcensEnforcerUrl=http://afcens-enforcer:3100/
redisHost=redis
mySqlHost=mysql
elasticsearchHost=elasticsearch

while [ $# -gt 0 ]; do
    case "$1" in
        --help)
            printHelp
            ;;
        --trustedUrlBase)
            urlBaseTrusted="$2"
            shift 2
            ;;
        --sandboxUrlBase)
            urlBaseSandbox="$2"
            shift 2
            ;;
        --withProxy)
            withProxy="$2"
            shift 2
            ;;
        --redisHost)
            redisHost="$2"
            shift 2
            ;;
        --elasticsearchHost)
            elasticsearchHost="$2"
            shift 2
            ;;
        --mySqlHost)
            mySqlHost="$2"
            shift 2
            ;;
        --afcensEnforcerUrl)
            afcensEnforcerUrl="$2"
            shift 2
            ;;
        *)
            echo "Error: unrecognized option $1."
            printHelp
    esac
done

cat > server/config/production.yaml <<EOT
www:
  host: 0.0.0.0
  proxy: $wwwProxy
  secret: "`pwgen -1`"

  trustedPort: 8080
  trustedPortIsHttps: false
  sandboxPort: 8081
  sandboxPortIsHttps: false
  apiPort: 8082
  apiPortIsHttps: false

  trustedUrlBase: $urlBaseTrusted
  sandboxUrlBase: $urlBaseSandbox

mysql:
  host: $mySqlHost

redis:
  enabled: true
  host: $redisHost

elasticsearch:
  host: $elasticsearchHost

log:
  level: info
  
enforcer:
  url: $afcensEnforcerUrl
EOT



# Wait for the other services to start
while ! nc -z $mySqlHost 3306; do sleep 1; done
while ! nc -z $redisHost 6379; do sleep 1; done
while ! nc -z $elasticsearchHost 9200; do sleep 1; done
while ! mysql -h mysql -u afcens --password=afcens -e 'show databases'; do sleep 1; done

cd server
NODE_ENV=production node index.js
