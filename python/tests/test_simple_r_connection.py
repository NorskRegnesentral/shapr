"""
Simple test to verify R connection and reproducibility.

This test bypasses all shaprpy functionality and just tests
if we can call R, sample 5 random numbers, and get consistent results.
"""
import pytest
import rpy2.robjects as robjects
import numpy as np


class TestSimpleRConnection:
    """Simple test for R connection and reproducibility."""

    def test_r_random_numbers_reproducibility(self):
        """Test that R produces reproducible random numbers."""
        # Set seed in R for reproducibility
        robjects.r('set.seed(123)')
        
        # Sample 5 random numbers from uniform distribution
        result1 = robjects.r('runif(5)')
        
        # Convert to numpy array for easier comparison
        numbers1 = np.array(result1)
        
        # Reset seed and sample again - should get same numbers
        robjects.r('set.seed(123)')
        result2 = robjects.r('runif(5)')
        numbers2 = np.array(result2)
        
        # Check that we get the same numbers
        np.testing.assert_array_almost_equal(numbers1, numbers2, decimal=10)
        
        # Also check that we get exactly 5 numbers
        assert len(numbers1) == 5
        assert len(numbers2) == 5
        
        # Check against expected values (these are the exact values we should get with seed 123)
        expected_numbers = np.array([0.28757752, 0.78830514, 0.40897692, 0.8830174, 0.94046728])
        np.testing.assert_array_almost_equal(numbers1, expected_numbers, decimal=7)
        
        print(f"Sampled numbers: {numbers1}")
        print(f"Expected numbers: {expected_numbers}")
        print(f"Numbers match expected: {np.allclose(numbers1, expected_numbers)}")
        print(f"Numbers are reproducible: {np.allclose(numbers1, numbers2)}")

    def test_r_normal_numbers_with_fixed_seed(self):
        """Test that R produces expected normal random numbers with fixed seed."""
        # Set seed in R
        robjects.r('set.seed(42)')
        
        # Sample 5 random numbers from normal distribution
        result = robjects.r('rnorm(5)')
        numbers = np.array(result)
        
        # Check that we get exactly 5 numbers
        assert len(numbers) == 5
        
        # Check against expected values (these are the exact values we should get with seed 42)
        expected_numbers = np.array([1.37095845, -0.56469817, 0.36312841, 0.6328626, 0.40426832])
        np.testing.assert_array_almost_equal(numbers, expected_numbers, decimal=7)
        
        print(f"Normal random numbers with seed 42: {numbers}")
        print(f"Expected numbers: {expected_numbers}")
        print(f"Numbers match expected: {np.allclose(numbers, expected_numbers)}")
        
        # Basic sanity checks
        assert all(np.isfinite(numbers))  # All numbers should be finite
        assert numbers.dtype == float  # Should be floating point numbers

    def test_r_version_info(self):
        """Test that we can get R version information."""
        version_result = robjects.r('R.version.string')
        version = str(version_result)
        print(f"R version: {version}")
        
        # Just check that we got some version string
        assert isinstance(version, str)
        assert len(version) > 0
        assert 'R version' in version
