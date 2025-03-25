{ config, lib, pkgs, ... }:

let
  gidek = pkgs.callPackage ../../package.nix { };
  cfgProgram = config.programs.gidek;
  cfgService = config.services.gidek;
in
{
  options = {
    programs.gidek = {
      enable = lib.mkEnableOption "gidek - Backup Git(Hub) Repositories";

      config = {
        store = lib.mkOption {
          type = lib.types.str;
          description = "Absolute path to store directory";
        };

        token = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "GitHub token";
        };

        token_file = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "GitHub token file (ignored if GitHub token is provided)";
        };

        repos = lib.mkOption {
          type = lib.types.listOf (lib.types.submodule {
            options.type = lib.mkOption {
              type = lib.types.enum [ "single" "user" "organization" ];
              description = "Type of GitHub repository source (single, user or organization)";
            };
            options.name = lib.mkOption {
              type = lib.types.str;
              description = "Type of GitHub repository source name.";
            };
          });
          description = "List of GitHub repository sources.";
        };
      };
    };

    services.gidek = {
      enable = lib.mkEnableOption "gidek - Backup Git(Hub) Repositories. Implies 'program.gidek = true;'.";
      user = lib.mkOption {
        type = lib.types.str;
        description = "User to run the service with.";
      };
      schedule = lib.mkOption {
        type = lib.types.str;
        description = "'systemd.timers.timerConfig.OnCalendar' value.";
      };
    };
  };

  config = {
    environment.systemPackages = lib.mkIf (cfgProgram.enable || cfgService.enable) [ gidek ];

    environment.etc = lib.mkIf (cfgProgram.enable || cfgService.enable) {
      "gidek/config.yaml" = {
        text = builtins.toJSON cfgProgram.config;
      };
    };

    systemd = lib.mkIf cfgService.enable {
      services.gidek = {
        serviceConfig = {
          Type = "oneshot";
          User = cfgService.user;
        };
        script = ''
          set -e
          gidek --config "/etc/gidek/config.yaml" backup
        '';
        path = [ gidek ];
      };

      timers.gidek = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = cfgService.schedule;
          Unit = "gidek.service";
        };
      };
    };
  };
}
