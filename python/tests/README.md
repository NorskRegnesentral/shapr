# shaprpy Test Suite

This directory contains the test suite for shaprpy, organized as follows:

## Structure

```
tests/
├── conftest.py                    # Shared pytest fixtures
├── unit/                          # Unit tests for individual components
│   └── test_input_validation.py   # Input validation and error handling tests
├── integration/                   # Integration tests with real models
│   ├── test_regression_snapshots.py    # Snapshot tests for regression models
│   └── test_classification_snapshots.py # Snapshot tests for classification models
└── snapshots/                     # Syrupy snapshot files (auto-generated)
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
# Run only unit tests
pytest tests/unit/

# Run only integration tests
pytest tests/integration/

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
pytest tests/integration/test_regression_snapshots.py --snapshot-update
```

## Test Categories

- **Unit Tests**: Fast tests that check individual components and input validation
- **Integration Tests**: Tests that run the full explain pipeline with real models
- **Snapshot Tests**: Tests that use Syrupy to ensure outputs remain consistent
- **Slow Tests**: Tests marked as slow (can be excluded with `-m "not slow"`)

## Adding New Tests

1. **Unit tests**: Add to `tests/unit/` for testing individual functions and validation
2. **Integration tests**: Add to `tests/integration/` for end-to-end testing
3. **Fixtures**: Add shared fixtures to `conftest.py`
4. **Mark slow tests**: Use `@pytest.mark.slow` for tests that take significant time

## CI/CD Integration

This test suite is designed to work with GitHub Actions. See the workflow files in `.github/workflows/` for CI configuration.
