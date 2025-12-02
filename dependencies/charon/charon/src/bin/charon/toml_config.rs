//! Processing of the contents of a `Charon.toml` file.
use charon_lib::options::CliOpts;
use serde::Deserialize;
use std::path::PathBuf;

/// The struct used to define the options available in `Charon.toml` files.
#[derive(Debug, Deserialize)]
pub struct TomlConfig {
    #[serde(default)]
    pub charon: CharonTomlConfig,
    #[serde(default)]
    pub rustc: RustcTomlConfig,
}

/// The struct used to define the options available in `Charon.toml` files. These all mirror the
/// corresponding cli option.
#[derive(Debug, Default, Deserialize)]
pub struct CharonTomlConfig {
    #[serde(default)]
    pub lib: bool,
    #[serde(default)]
    pub bin: Option<String>,
    #[serde(default)]
    pub mir_promoted: bool,
    #[serde(default)]
    pub mir_optimized: bool,
    #[serde(default)]
    pub polonius: bool,
    #[serde(default)]
    pub extract_opaque_bodies: bool,
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub opaque: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub no_merge_goto_chains: bool,
}

#[derive(Debug, Default, Deserialize)]
pub struct RustcTomlConfig {
    #[serde(default)]
    pub flags: Vec<String>,
}

impl TomlConfig {
    /// Applies the options specified in the toml file to the cli options. In case of conflict, cli
    /// options take precedence.
    pub(crate) fn apply(self, mut config: CliOpts) -> CliOpts {
        config.lib |= self.charon.lib;
        config.bin = config.bin.or(self.charon.bin);
        config.mir_promoted |= self.charon.mir_promoted;
        config.mir_optimized |= self.charon.mir_optimized;
        config.use_polonius |= self.charon.polonius;
        config.extract_opaque_bodies |= self.charon.extract_opaque_bodies;
        config.include.extend(self.charon.include);
        config.opaque.extend(self.charon.opaque);
        config.exclude.extend(self.charon.exclude);
        config.rustc_args.extend(self.rustc.flags);
        config.no_merge_goto_chains |= self.charon.no_merge_goto_chains;
        config
    }
}

/// Read `./Charon.toml` if there is such a file.
pub(crate) fn read_toml() -> Option<TomlConfig> {
    trace!("Reading options from the `Charon.toml` file");
    let path = PathBuf::from("./Charon.toml");
    if path.exists() {
        let contents = std::fs::read_to_string(path).unwrap();
        Some(toml::from_str(&contents).unwrap())
    } else {
        None
    }
}
