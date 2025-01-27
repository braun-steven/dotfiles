try:
    import torch
    print(f"PyTorch {torch.__version__} imported.")

#     # Save the original __repr__ method so we can call it if needed
#     original_tensor_repr = torch.Tensor.__repr__

#     # Define a new __repr__ method to show both the shape and the data in a custom format
#     def custom_tensor_repr(self):
#         # Get the original representation
#         original_repr = original_tensor_repr(self)
#         # Return a string with the tensor's shape and data in the desired format
#         return f"Tensor(\n  shape={list(self.shape)},\n  data={original_repr}\n)"

#     # Monkey-patch the __repr__ method
#     torch.Tensor.__repr__ = custom_tensor_repr
except:
    print("PyTorch not installed.")

try:
    import numpy as np
    print(f"NumPy {np.__version__} imported.")
except:
    print("NumPy not installed.")
