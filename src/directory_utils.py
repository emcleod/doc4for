import os
import shutil
import tempfile
import errno
import time
import random

#TODO what about large directories

def create_or_clear_directory(directory_path, max_retries=5, base_delay=0.1, preserve_subdirs=None):
    """
    Creates a directory if it doesn't exist or clears its contents if it does.
    Optionally preserves specified subdirectories.

    Args:
        directory_path (str): Path to the directory to create or clear.
        max_retries (int): Maximum number of retry attempts.
        base_delay (float): Base delay between retries, in seconds.
        preserve_subdirs (List[str], optional): List of subdirectories to preserve when clearing.

    Raises:
        PermissionError: If the program doesn't have write permissions.
        OSError: If there's an issue creating the directory or removing its contents after all retries.
    """
    if not check_write_permissions(os.path.dirname(directory_path)):
        raise PermissionError(f"No write permissions in {os.path.dirname(directory_path)}.")

    for attempt in range(max_retries):
        try:
            if not os.path.exists(directory_path):
                os.makedirs(directory_path)
            else:
                clear_directory(directory_path, preserve_subdirs)
            return  # Success, exit the function
        except FileExistsError:
            # Directory was created by another process after we checked
            continue
        except FileNotFoundError:
            # Directory was deleted by another process after we checked
            continue
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise
        
        # If we get here, we need to retry
        time.sleep(base_delay * (2 ** attempt) * (random.random() + 0.5))

    # If we've exhausted all retries, make one last attempt and let any exceptions propagate
    if not os.path.exists(directory_path):
        os.makedirs(directory_path)
    else:
        clear_directory(directory_path, preserve_subdirs)

def clear_directory(directory, preserve_subdirs=None):
    """
    Clears the contents of the given directory, optionally preserving specified subdirectories.

    Args:
        directory (str): Path to the directory to clear.
        preserve_subdirs (List[str], optional): List of subdirectories to preserve.

    Raises:
        OSError: If there's an issue removing the contents.
    """
    for item in os.listdir(directory):
        if preserve_subdirs and item in preserve_subdirs:
            continue
        item_path = os.path.join(directory, item)
        if os.path.islink(item_path) or os.path.isfile(item_path):
            os.unlink(item_path)
        elif os.path.isdir(item_path):
            shutil.rmtree(item_path)

def check_write_permissions(path):
    """
    Check if the program has write permissions in the specified path.

    Args:
        path (str): The path to check for write permissions.

    Returns:
        bool: True if write permissions are available, False otherwise.
    """
    try:
        testfile = tempfile.TemporaryFile(dir=path)
        testfile.close()
    except (IOError, OSError):
        return False
    return True

