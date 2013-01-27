if [[ "$OSTYPE" == linux* ]]; then
  eval $(keychain --eval --agents ssh -Q --quiet id_rsa)
fi

