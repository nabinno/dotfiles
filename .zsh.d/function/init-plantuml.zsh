export REQUIRED_GRAPHVIZ_VERSION=2.38
case $OSTYPE in
  msys) export PATH=/C/Program~2/Graphviz${REQUIRED_GRAPHVIZ_VERSION}/bin:$PATH ;;
  cygwin) export PATH=/cygdrive/C/Program~2/Graphviz${REQUIRED_GRAPHVIZ_VERSION}/bin:$PATH ;;
esac

function get-puml() {
  case "${OSTYPE}" in
    freebsd* | darwin*) ;;
    linux*) if type -p npm >/dev/null; then npm install -g node-plantuml; fi ;;
  esac
}

function get-plantuml() {
  case "${OSTYPE}" in
    freebsd* | darwin*) ;;
    linux*) wget http://jaist.dl.sourceforge.net/project/plantuml/plantuml.8027.jar -O ~/.local/bin/plantuml.jar ;;
  esac
}

function get-graphviz() {
  case "${OSTYPE}" in
    msys | cygwin) choco install graphviz ;;
    freebsd* | darwin*) brew install graphviz ;;
    linux*)
      case "${DIST}" in
        Redhat | RedHat) sudo yum install -y graphviz ;;
        Debian | Ubuntu)
          sudo apt-get update -y
          sudo apt-get install -y graphviz
          ;;
      esac
      ;;
  esac
}

if ! type -p puml >/dev/null; then get-puml; fi
if [ ! -f ~/.local/bin/plantuml.jar ]; then get-plantuml; fi
if [ -f ~/.local/bin/plantuml.jar ]; then alias plantuml='java -jar ~/.local/bin/plantuml.jar -tpng'; fi
if ! type -p dot >/dev/null; then get-graphviz; fi
