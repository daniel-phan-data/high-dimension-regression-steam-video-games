import subprocess

scripts = [
    "setup.py",
    "univariate_tests.py",
    "univariate_tests_log.py",
    "bivariate_tests.py",
    "correlation_matrix.py",
    "univ_rating.py",
    "lm_and_hypotheses.py",
    "lm_selection.py",
    "classification.py"
]

for script in scripts:
    print(f"Running {script}...")
    subprocess.run(["uv", "run", script])
