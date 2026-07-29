use std::fs::{self, File, Permissions};
use std::io::{self, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use tempfile::{NamedTempFile, tempdir_in};

const DIRECTORY_MODE: u32 = 0o755;
const FILE_MODE: u32 = 0o644;
const EXECUTABLE_MODE: u32 = 0o755;

struct Asset {
    relative_path: &'static str,
    contents: &'static [u8],
}

const ASSETS: &[Asset] = &[
    Asset {
        relative_path: "config.example.toml",
        contents: include_bytes!("../../../config.example.toml"),
    },
    Asset {
        relative_path: "lua/blink-cmp-shell-sense/init.lua",
        contents: include_bytes!("../../../lua/blink-cmp-shell-sense/init.lua"),
    },
    Asset {
        relative_path: "shell/bash/client.bash",
        contents: include_bytes!("../../../shell/bash/client.bash"),
    },
    Asset {
        relative_path: "shell/bash/provider.bash",
        contents: include_bytes!("../../../shell/bash/provider.bash"),
    },
    Asset {
        relative_path: "shell/bash/shell-sense.bash",
        contents: include_bytes!("../../../shell/bash/shell-sense.bash"),
    },
    Asset {
        relative_path: "shell/fish/client.fish",
        contents: include_bytes!("../../../shell/fish/client.fish"),
    },
    Asset {
        relative_path: "shell/fish/provider.fish",
        contents: include_bytes!("../../../shell/fish/provider.fish"),
    },
    Asset {
        relative_path: "shell/fish/shell-sense.fish",
        contents: include_bytes!("../../../shell/fish/shell-sense.fish"),
    },
    Asset {
        relative_path: "shell/zsh/client.zsh",
        contents: include_bytes!("../../../shell/zsh/client.zsh"),
    },
    Asset {
        relative_path: "shell/zsh/provider.zsh",
        contents: include_bytes!("../../../shell/zsh/provider.zsh"),
    },
    Asset {
        relative_path: "shell/zsh/shell-sense.plugin.zsh",
        contents: include_bytes!("../../../shell/zsh/shell-sense.plugin.zsh"),
    },
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstallPaths {
    pub executable: PathBuf,
    pub data: PathBuf,
}

pub fn install(source_executable: &Path, paths: &InstallPaths) -> Result<()> {
    if !source_executable.is_file() {
        bail!(
            "installer executable does not exist at {}",
            source_executable.display()
        );
    }
    install_assets(&paths.data)?;
    install_executable(source_executable, &paths.executable)?;
    Ok(())
}

fn install_assets(destination: &Path) -> Result<()> {
    let parent = destination
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .context("installation data path has no parent")?;
    create_directory(parent)?;
    let transaction = tempdir_in(parent).with_context(|| {
        format!(
            "could not create an installation transaction in {}",
            parent.display()
        )
    })?;
    let payload = transaction.path().join("payload");
    create_directory(&payload)?;
    for asset in ASSETS {
        write_asset(&payload.join(asset.relative_path), asset.contents)?;
    }

    let previous = transaction.path().join("previous");
    let had_previous = match destination.symlink_metadata() {
        Ok(_) => true,
        Err(error) if error.kind() == io::ErrorKind::NotFound => false,
        Err(error) => {
            return Err(error).with_context(|| {
                format!(
                    "could not inspect the previous installation at {}",
                    destination.display()
                )
            });
        }
    };
    if had_previous {
        fs::rename(destination, &previous).with_context(|| {
            format!(
                "could not move the previous installation at {}",
                destination.display()
            )
        })?;
    }
    if let Err(error) = fs::rename(&payload, destination) {
        if had_previous {
            fs::rename(&previous, destination).with_context(|| {
                format!(
                    "installation failed and the previous tree at {} could not be restored",
                    destination.display()
                )
            })?;
        }
        return Err(error).with_context(|| {
            format!(
                "could not publish the installation at {}",
                destination.display()
            )
        });
    }
    Ok(())
}

fn install_executable(source: &Path, destination: &Path) -> Result<()> {
    if !source.is_file() {
        bail!(
            "installer executable does not exist at {}",
            source.display()
        );
    }
    let parent = destination
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .context("installation binary path has no parent")?;
    create_directory(parent)?;
    let mut staged = NamedTempFile::new_in(parent).with_context(|| {
        format!(
            "could not stage the executable in installation directory {}",
            parent.display()
        )
    })?;
    let mut source_file = File::open(source)
        .with_context(|| format!("could not open installer executable {}", source.display()))?;
    io::copy(&mut source_file, staged.as_file_mut())
        .context("could not copy the installer executable")?;
    staged
        .as_file_mut()
        .set_permissions(Permissions::from_mode(EXECUTABLE_MODE))
        .context("could not set installed executable permissions")?;
    staged
        .as_file_mut()
        .sync_all()
        .context("could not flush the installed executable")?;
    staged
        .persist(destination)
        .map_err(|error| error.error)
        .with_context(|| {
            format!(
                "could not publish the executable at {}",
                destination.display()
            )
        })?;
    Ok(())
}

fn write_asset(path: &Path, contents: &[u8]) -> Result<()> {
    let parent = path
        .parent()
        .context("embedded installation asset has no parent")?;
    create_directory(parent)?;
    let mut file = File::create(path)
        .with_context(|| format!("could not create installed asset {}", path.display()))?;
    file.write_all(contents)
        .with_context(|| format!("could not write installed asset {}", path.display()))?;
    file.set_permissions(Permissions::from_mode(FILE_MODE))
        .with_context(|| format!("could not set permissions on asset {}", path.display()))?;
    file.sync_all()
        .with_context(|| format!("could not flush installed asset {}", path.display()))?;
    Ok(())
}

fn create_directory(path: &Path) -> Result<()> {
    match path.symlink_metadata() {
        Ok(metadata) if metadata.is_dir() => return Ok(()),
        Ok(_) => bail!(
            "installation directory path is not a directory: {}",
            path.display()
        ),
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => {
            return Err(error).with_context(|| {
                format!(
                    "could not inspect installation directory {}",
                    path.display()
                )
            });
        }
    }
    fs::create_dir_all(path)
        .with_context(|| format!("could not create installation directory {}", path.display()))?;
    fs::set_permissions(path, Permissions::from_mode(DIRECTORY_MODE)).with_context(|| {
        format!(
            "could not set permissions on installation directory {}",
            path.display()
        )
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asset_installation_replaces_the_owned_tree() {
        let temporary = tempfile::tempdir().unwrap();
        let destination = temporary.path().join("share/shell-sense");
        create_directory(&destination).unwrap();
        fs::write(destination.join("stale-owned-file"), b"old").unwrap();

        install_assets(&destination).unwrap();

        assert!(!destination.join("stale-owned-file").exists());
        for asset in ASSETS {
            assert_eq!(
                fs::read(destination.join(asset.relative_path)).unwrap(),
                asset.contents
            );
        }
    }

    #[test]
    fn executable_installation_is_atomic_and_executable() {
        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("source");
        let destination = temporary.path().join("bin/shell-sense");
        fs::write(&source, b"new executable").unwrap();
        create_directory(destination.parent().unwrap()).unwrap();
        fs::write(&destination, b"old executable").unwrap();

        install_executable(&source, &destination).unwrap();

        assert_eq!(fs::read(&destination).unwrap(), b"new executable");
        let mode = destination.metadata().unwrap().permissions().mode();
        assert_eq!(mode & 0o777, EXECUTABLE_MODE);
    }
}
