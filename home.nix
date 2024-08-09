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

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
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
    google-cloud-sdk
    k9s
    postgresql_16
    tmuxinator
  ];



  home.file = {
    ".config/awesome/rc.lua".source = ~/git/dots/awesome/rc.lua;
    ".emacs.d/init.el".source = ~/git/dots/emacs.d/init.el;
    ".emacs.d/early-init.el".source = ~/git/dots/emacs.d/early-init.el;
    ".emacs.d/navapali-maps.el".source = ~/git/dots/emacs.d/navapali-maps.el;
    ".emacs.d/packages".source = ~/git/dots/emacs.d/packages;
    ".emacs.d/copilot.el".source = ~/git/dots/emacs.d/copilot.el;
    ".emacs.d/snippets".source = ~/git/dots/emacs.d/snippets;
    ".config/bspwm/bspwmrc".source = ~/git/dots/bspwm/bspwmrc;
    ".config/sxhkd/sxhkdrc".source = ~/git/dots/bspwm/sxhkdrc;
    ".config/hypr/hyprland.conf".source = ~/git/dots/hypr/hyprland.conf;
    ".config/waybar/config.jsonc".source = ~/git/dots/waybar/config.jsonc;
    ".config/waybar/style.css".source = ~/git/dots/waybar/style.css;
    ".Xmodmap".source = ~/git/dots/Xmodmap;
  };

  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
