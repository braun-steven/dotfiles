try:
    import torch
    print(f"PyTorch {torch.__version__} imported.")
except:
    print("PyTorch not installed.")

try:
    import numpy as np
    print(f"NumPy {np.__version__} imported.")
except:
    print("NumPy not installed.")
