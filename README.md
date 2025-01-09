# doc4for
A documentation generator for Fortran code.

## Configuration
doc4for uses a JSON configuration file to customize its behavior. By default, it looks for doc4for.json in the current directory, but you can specify a different location:

```bash
doc4for -c /path/to/your/config.json
```
### Configuration File Structure
Here's a basic configuration file:

```json
{
  "output_dir": "docs",
  "extensions": ["f90", "f95", "f03", "f08", "f77"],
  "output_formats": {
    "html": {
      "enabled": true,
      "templates": {
        "root_dir": "templates",
        "static": {
          "css": "static/css",
          "images": "static/images"
        }
      },
      "syntax_highlighting": true,
      "template_dir": null,
      "single_page": false
    },
    "pdf": {
      "enabled": false
    },
    "markdown": {
      "enabled": false
    }
  },
  "exclude_dirs": ["docs", ".git", "__pycache__", "build", "dist", "myenv"],
  "author": null,
  "title": null,
  "version": null,
  "include_private": false
}
```

### Configuration Options
* `output_dir`: Directory where documentation will be generated (default: "docs")
* `extensions`: List of Fortran file extensions to process
* `exclude_dirs`: Directories to ignore when searching for Fortran files
* `author`: (Optional) Documentation author name
* `title`: (Optional) Documentation title
* `version`: (Optional) Version of the documented code
* `include_private`: Whether to include private entities in the documentation (default: false)

### HTML Output Options
* `enabled`: Whether to generate HTML documentation
* `templates`: Configuration for template directories
  * `root_dir`: Root directory for templates
  * `static`: Directories for static assets
    * `css`: Directory for CSS files
    * `images`: Directory for image files
* `syntax_highlighting`: Enable syntax highlighting in code blocks
* `template_dir`: Custom template directory
* `single_page`: Generate all documentation in a single HTML file

### Future Output Formats
Support for PDF and Markdown output is planned for future releases. Currently, these options
are included in the configuration but are not functional.

## Default Behavior
If no configuration file is found, doc4for will use sensible defaults:

* Look for `.f90`, `.f95`, `.f03`, `.f08`, and `.f77` files
* Generate HTML documentation
* Output to a docs directory
* Exclude common directories like `.git`, `__pycache__`, etc.
* Use default templates and styling

## Example Usage
1. Create a `doc4for.json` file in your project directory
2. Customize the options as needed
3. Run:
```bash
doc4for
```
Or with a custom config location:
```bash
doc4for -c myconfig.json
```
## Installation
```bash
pip install doc4for
```
## License
