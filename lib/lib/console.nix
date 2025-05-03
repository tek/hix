{self}: let

  inherit (self) lib;

  console = import ../console.nix { inherit lib; };

  messageStream = ">& $_hix_msg_fd";

  loadConsoleWith = {verbose ? true}: let

    inherit (console) colors aliases chevrons chevronY chevronM chevronsH startSgr resetSgr;

    colorFun = name: ''
    ${name}()
    {
      _hix_nest_sgr "${startSgr colors.${name}}" "$*"
    }
    '';

    startColor = name: ''
    ${name}_start="${startSgr colors.${name}}"
    '';

    colorAliasFun = alias: for: ''
    color_${alias}()
    {
      ${for} "$*"
    }
    '';

  in ''
  _hix_console_is_zsh()
  {
    [[ -n "''${ZSH_NAME:-}" ]]
  }

  hix_cat()
  {
    cat $* ${messageStream}
  }

  _hix_echo()
  {
    echo "$@" ${messageStream}
  }

  if _hix_console_is_zsh
  then
    hix_print_raw()
    {
      $=_hix_printer_raw "$@"
    }
    hix_print()
    {
      $=_hix_printer "$@"
    }
    _hix_redirect()
    {
      $=* ${messageStream}
    }
  else
    hix_print_raw()
    {
      $_hix_printer_raw "$@"
    }
    hix_print()
    {
      $_hix_printer "$@"
    }
    _hix_redirect()
    {
      $* ${messageStream}
    }
  fi

  export _hix_printer_raw=''${_hix_printer_raw:-_hix_echo}
  export _hix_printer=''${_hix_printer:-_hix_echo -e}
  export _hix_msg_fd=2
  export _hix_verbose=${if verbose then "1" else "0"}

  reset_sgr="${resetSgr}"
  bold_start="${startSgr 1}"
  chevronsH='${chevronsH}'
  chevrons='${chevrons}'
  chevronY='${chevronY}'
  chevronM='${chevronM}'

  ${self.unlines (map colorFun (lib.attrNames colors))}
  ${self.unlines (map startColor (lib.attrNames colors))}
  ${self.unlines (lib.mapAttrsToList colorAliasFun aliases)}

  # SGR 28 is "Reveal"
  _hix_color_marker=$(echo -e "${startSgr 28}")

  # Place a marker after the colored string, and replace it in substrings by the current SGR code to allow easy nesting
  # of colors.
  # The marker stays in to allow multiple levels (for bold + fg + bg), but since it is a no-op SGR itself that's fine.
  _hix_nest_sgr()
  {
    local sgr=$1
    local string=$2
    local nest="''${string//''${_hix_color_marker}/$_hix_color_marker$sgr}"
    echo -e "$sgr$nest${resetSgr}$_hix_color_marker"
  }

  _hix_sanitize_sgr()
  {
    echo "''${*//''${_hix_color_marker}/}"
  }

  bold()
  {
    _hix_nest_sgr "${startSgr 1}" "$*"
  }

  _hix_message_fragment()
  {
    hix_print -n "$(_hix_sanitize_sgr "$*")"
  }

  _hix_message()
  {
    hix_print "$(_hix_sanitize_sgr "$chevrons $*")"
  }

  _hix_message_part()
  {
    hix_print -n "$(_hix_sanitize_sgr "$chevrons $*")"
  }

  message_fragment()
  {
    if (( $_hix_verbose == 1 ))
    then
      _hix_message_fragment "$*"
    fi
  }

  message()
  {
    if (( $_hix_verbose == 1 ))
    then
      _hix_message "$*"
    fi
  }

  message_part()
  {
    if (( $_hix_verbose == 1 ))
    then
      _hix_message_part "$*"
    fi
  }

  message_hang()
  {
    message "  $*"
  }

  message_part_hang()
  {
    message_part "  $*"
  }

  error_message()
  {
    message "$(color_error $*)"
  }

  error_message_hang()
  {
    error_message "  $*"
  }

  error_message_part_hang()
  {
    _hix_message_part "  $*"
  }

  die()
  {
    error_message $*
    exit 1
  }

  if _hix_console_is_zsh
  then
    ask()
    {
      setopt local_options no_err_exit no_err_return
      local decision=""
      message_part "$1 [Yn] "
      read -k decision
      if [[ $decision != '\n' ]]
      then
        hix_print
      fi
      [[ $decision != 'n' ]]
    }
  fi
  '';

  loadConsole = loadConsoleWith {};

in {

  inherit
  console
  messageStream
  loadConsoleWith
  loadConsole
  ;

  inherit (console.s) colors colorsBg;

}
