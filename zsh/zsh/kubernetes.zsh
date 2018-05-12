if command_exists kubectl; then
  source <(kubectl completion zsh)
fi

if command_exists minikube; then
  source <(minikube completion zsh)
fi

if command_exists helm; then
  source <(helm completion zsh)
fi
