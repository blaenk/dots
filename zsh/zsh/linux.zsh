if [[ "$OSTYPE" == linux* ]]; then
  eval $(ssh-agent)
  ssh-add
fi

