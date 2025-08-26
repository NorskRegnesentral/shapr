# shaprpy Test Suite

This directory contains the test suite for shaprpy, organized as follows:

## Structure

```
tests/
├── conftest.py                    # Shared pytest fixtures
├── setup/                         # Unit tests for setup and validation
│   └── test_input_validation.py   # Input validation and error handling tests
├── output/                        # Output tests for explanation results
│   ├── test_regression_outputs.py    # Output tests for regression models
│   ├── test_classification_outputs.py # Output tests for classification models
│   └── __snapshots__/             # Syrupy snapshot files (auto-generated)
└── README.md                      # This file
```

## Running Tests

### Install test dependencies
```bash
cd python
pip install -e ".[test]"
```

### Run all tests
```bash
cd python
pytest
```

### Run specific test categories
```bash
# Run only setup tests
pytest tests/setup/

# Run only output tests
pytest tests/output/

# Run only snapshot tests
pytest -m snapshot

# Run fast tests only (exclude slow tests)
pytest -m "not slow"
```

### Update snapshots
```bash
# Update all snapshots
pytest --snapshot-update

# Update specific test snapshots
pytest tests/output/test_regression_outputs.py --snapshot-update
```

## Test Categories

- **Setup Tests**: Fast tests that check individual components and input validation
- **Output Tests**: Tests that use Syrupy to ensure explanation outputs remain consistent
- **Snapshot Tests**: Tests marked with `@pytest.mark.snapshot` (subset of output tests)
- **Slow Tests**: Tests marked as slow (can be excluded with `-m "not slow"`)

## Adding New Tests

1. **Setup tests**: Add to `tests/setup/` for testing individual functions and validation
2. **Output tests**: Add to `tests/output/` for end-to-end output consistency testing
3. **Fixtures**: Add shared fixtures to `conftest.py`
4. **Mark slow tests**: Use `@pytest.mark.slow` for tests that take significant time

## CI/CD Integration

This test suite is designed to work with GitHub Actions. See the workflow files in `.github/workflows/` for CI configuration.
