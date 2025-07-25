{ config, pkgs, inputs,  ... }:
let emacs = pkgs.emacsWithPackagesFromUsePackage {
      config = ../emacs.d/init.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      package = pkgs.emacs-git;
      extraEmacsPackages = epkgs: [
      ];
    };
    nvidiaPackage = config.boot.kernelPackages.nvidiaPackages.legacy_390;
    nvidiaX11Package = pkgs.linuxKernel.packages.linux_5_15.nvidia_x11_legacy390;
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];


  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  nix.settings.trusted-users = ["root" "@wheel"];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelModules = ["nvidia"];
  boot.initrd.kernelModules = ["nvidia"];
  boot.extraModulePackages = [nvidiaPackage];
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_5_15;
  
  hardware.enableRedistributableFirmware = true;


  ## Networking logic
  networking.hostName = "ayys"; # Define your hostname.
  networking.networkmanager.enable = true;
  

  # Set your time zone.
  time.timeZone = "Asia/Kathmandu";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.inputMethod = {
    enable = true;
    type = "ibus";    # Set IBus as the input method framework
    ibus.engines = with pkgs.ibus-engines; [
      m17n   # Enable m17n engine (which supports typing in Devanagari)
    ];
  };

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.defaultSession = "none+bspwm";
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.wayland.enable = false;
  
  
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = emacs;
  };

  # Configure keymap in X11
  services.xserver = {
    videoDrivers = ["nvidiaLegacy390"];
    enable = true;
    xkb = {
      layout = "us";
      variant = "";
      options = "ctrl:nocaps";
    };
    exportConfiguration = true;
    modules = [pkgs.linuxKernel.packages.linux_5_15.nvidia_x11_legacy390_patched.bin];
    extraConfig = ''
      Section "Device"
        Identifier "NvidiaCard"
        Driver     "nvidia"
        BusID      "PCI:7:0:0"
      EndSection
    '';
    windowManager = {
      awesome = {
        enable = true;
        luaModules = with pkgs.luaPackages; [
          luarocks # is the package manager for Lua modules
          luadbi-mysql # Database abstraction layer
        ];
      };
      bspwm = {
        enable = true;
      };
    };
  };
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [nvidiaX11Package.out];
    extraPackages32 = [nvidiaX11Package.lib32];
  };
  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    nvidiaSettings = true;
    package = nvidiaPackage;
  };

  # Enable CUPS to print documents.
  services.printing.enable = false;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ayys = {
    isNormalUser = true;
    description = "Ayush";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
      (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
      aspell
      atuin
      autoconf
      automake
      bacon
      bison
      cascadia-code
      chromium
      cmake
      corepack
      delta
      devenv
      dig
      dina-font
      docker
      doppler
      editorconfig-core-c
      extra-cmake-modules
      eza
      feh
      file
      fira-code
      fira-code-symbols
      firefox
      fd
      fmt
      fortune
      gcc
      gdb
      gdbgui
      gettext
      gh
      gnumake
      go
      go-font
      gperf
      guix
      hack-font
      hackgen-nf-font
      htop
      hydroxide
      inkscape
      inputs.ayys-st.packages."${pkgs.system}".st
      inputs.ayys-uv.packages."${pkgs.system}".uv
      inputs.wasmenv.packages."${pkgs.system}".wasmenv
      insomnia
      jq
      json_c
      k9s
      kdePackages.dolphin
      kitty
      kubectl
      lemonbar
      libcap
      liberation_ttf
      libtool
      live-server
      lohit-fonts.devanagari
      m4
      man-pages
      man-pages-posix
      meson
      mplayer
      mplus-outline-fonts.githubRelease
      mysql84
      nasm
      nerd-fonts.droid-sans-mono
      nerd-fonts.fira-code
      nerd-fonts.hack
      nix-index
      nodejs
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      nyxt
      pavucontrol
      pkg-config
      postgresql_16
      postman
      proggyfonts
      pyenv
      python311Packages.ipython
      python311Packages.pip
      python311Packages.pylsp-mypy
      python311Packages.python
      python311Packages.python-lsp-ruff
      python311Packages.python-lsp-server
      python311Packages.requests
      python311Packages.ruff
      qemu_kvm
      redis
      ripgrep
      rofi
      slack
      source-code-pro
      sxhkd
      pay-respects
      tmux
      tmuxinator
      tree
      unzip
      victor-mono
      vistafonts
      wasmer
      xcape
      xclip
      xdiskusage
      xdo
      xorg.libX11
      xorg.libX11.dev
      xorg.libXft
      xorg.libXinerama
      xorg.xmodmap
      xtitle
      yarn
      yq
      zip
      code-cursor
    ];
  };


  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = false;
  services.displayManager.autoLogin.user = "ayys";

  # programs.hyprland = {
  #   enable = true; 
  #   xwayland.enable = true;
  # };
  # programs.waybar.enable = true;

  # # Hint Electon apps to use wayland
  # environment.sessionVariables = {
  #   LD_LIBRARY_PATH =  [ "/run/opengl-driver/lib" ];
  # };
  services.dbus.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    killall
    inputs.rust-overlay.packages.x86_64-linux.stable.toolchain
    nvidiaPackage
  ];

  documentation.dev.enable = true;

  documentation.man = {
    man-db.enable = false;
    mandoc.enable = true;
  };
  
  nixpkgs.overlays = [
    (_: super: let pkgs = inputs.rust-overlay.inputs.nixpkgs.legacyPackages.${super.system}; in inputs.rust-overlay.overlays.default pkgs pkgs)

    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/147f6b98f17b0d66866eb8923a6ae6fe9c23b65e.tar.gz";
      sha256 = "0r5q63rf6i4sm5aaxd915731vbfihhfslnhayr41kmbgkvm02ihr";
    }))

    (self: super: {
      linuxKernel = super.linuxKernel // {
        packages = super.linuxKernel.packages // {
          linux_5_15 = super.linuxKernel.packages.linux_5_15.extend (kernelSelf: kernelSuper: {
            nvidia_x11_legacy390_patched = kernelSuper.nvidia_x11_legacy390.overrideAttrs (old: {
              postInstall = (old.postInstall or "") + ''
              ext_dir="$bin/lib/xorg/modules/extensions"
              target=$(find "$ext_dir" -name 'libglx.so.*' | head -n1)
              if [ -n "$target" ]; then
                ln -sf "$(basename "$target")" "$ext_dir/libglx.so"
              fi
            '';
            });
          });
        };
      };
    })

  ];

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # Add any missing dynamic libraries for unpackaged programs
    # here, NOT in environment.systemPackages
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;


  programs.direnv.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  systemd.user.services.xrandr = {
    description = "Set xrandr settings on startup";
    serviceConfig = {
      ExecStart = "${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-1 --rotate normal";
    };
    wantedBy = [ "graphical-session.target" ];
  };

  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart = "${pkgs.guix.out}/bin/guix-daemon --build-users-group=guixbuild --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org'";
      Environment="GUIX_LOCPATH=/root/.guix-profile/lib/locale";
      RemainAfterExit="yes";
      StandardOutput="syslog";
      StandardError="syslog";
      TaskMax= "8192";
    };
    wantedBy = [ "multi-user.target" ];
  };



  nix.settings.auto-optimise-store = true;
  nix.optimise.automatic = true;


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.nginx = {
    enable = true;
    virtualHosts = {
      "frontend.local" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:3000";
        };
      };
      "backend.local" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:8000";
        };
      };
    };
  };

  fonts.fontconfig.enable = true;

  virtualisation.docker.enable = true;

  networking.nameservers = [ "1.1.1.1" "9.9.9.9" ];
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8000 3000 22 ];
  networking.extraHosts = ''
127.0.0.1 frontend.local
127.0.0.1 backend.local
127.0.0.1 www.crysys.hu
  '';
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
