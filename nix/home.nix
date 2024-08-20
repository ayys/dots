{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.

  home = {
    # The username for the user to create.
    username = "ayys";

    # The home directory for the user to create.
    homeDirectory = "/home/ayys";
  };

  # Install unfree programs
  # nix.package = pkgs.nix;
  nixpkgs = {
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.packages = with pkgs; [
    htop
    git
    fortune
    editorconfig-core-c
    ripgrep
    nodejs
    yarn
    slack
    tmux
    tmuxinator
    pyenv
    cmake
    aspell
    kubectl
    (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
    k9s
    postgresql_16
    tmuxinator
    atuin
    eza
    gnumake
    gcc
    libtool
    delta
    gh
    rustup
    devenv
    redis
    docker
    doppler
    kitty
    rofi
    wasmer
    steam-run
    corepack
  ];

  home.file = {
    ".config/awesome/rc.lua".source = ../awesome/rc.lua;
    ".emacs.d/init.el".source = ../emacs.d/init.el;
    ".emacs.d/early-init.el".source = ../emacs.d/early-init.el;
    ".emacs.d/navapali-maps.el".source = ../emacs.d/navapali-maps.el;
    ".emacs.d/copilot.el".source = ../emacs.d/copilot.el;
    ".emacs.d/snippets".source = ../emacs.d/snippets;
    ".config/bspwm/bspwmrc".source = ../bspwm/bspwmrc;
    ".config/sxhkd/sxhkdrc".source = ../bspwm/sxhkdrc;
    ".config/hypr/hyprland.conf".source = ../hypr/hyprland.conf;
    ".config/waybar/config.jsonc".source = ../waybar/config.jsonc;
    ".config/waybar/style.css".source = ../waybar/style.css;
    ".config/kitty/kitty.conf".source = ../kitty/kitty.conf;
    ".Xmodmap".source = ../Xmodmap;
  };

  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
  home.shellAliases = {
    rebuild = "nixos-rebuild --flake $HOME/git/dots/nix#ayys --use-remote-sudo switch";
    git-rm-ws = "git diff -U0 -w --no-color | git apply --cached --ignore-whitespace --unidiff-zero -";
  };
  programs.zoxide.enable = true;
  programs.bash = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName  = "ayys";
    extraConfig = {
      push = { autoSetupRemote = true; };
    };

  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
