import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import warnings

warnings.filterwarnings('ignore')

torch.manual_seed(42)
np.random.seed(42)


class SOCDataset(Dataset):
    def __init__(self, csv_file, transform=None):
        """
        Initialize the dataset
        Args:
            csv_file: CSV filepath
            transform: Data Conversion
        """

        data = pd.read_csv(csv_file)


        self.labels = data.iloc[:, 0].values.astype(np.float32)  # SOC列
        features = data.iloc[:, 1:18].values.astype(np.float32)  # 17个特征列


        num_samples = features.shape[0]
        self.features = features.reshape(num_samples, 17, 9, 9)


        self.scaler = StandardScaler()

        original_shape = self.features.shape
        flattened = self.features.reshape(num_samples, -1)
        normalized = self.scaler.fit_transform(flattened)
        self.features = normalized.reshape(original_shape)


        self.label_scaler = StandardScaler()
        self.labels = self.label_scaler.fit_transform(self.labels.reshape(-1, 1)).flatten()

        self.transform = transform

    def __len__(self):
        return len(self.features)

    def __getitem__(self, idx):
        feature = self.features[idx]
        label = self.labels[idx]

        if self.transform:
            feature = self.transform(feature)


        feature_tensor = torch.FloatTensor(feature)
        label_tensor = torch.FloatTensor([label])

        return feature_tensor, label_tensor



class SOC_CNN(nn.Module):
    def __init__(self, input_channels=17):
        super(SOC_CNN, self).__init__()


        self.conv1 = nn.Conv2d(
            in_channels=input_channels,
            out_channels=32,
            kernel_size=3,  # 3x3卷积核
            padding=0
        )
        self.relu1 = nn.ReLU()


        self.pool1 = nn.MaxPool2d(kernel_size=2, stride=2)


        self.conv2 = nn.Conv2d(
            in_channels=32,
            out_channels=64,
            kernel_size=3,
            padding=0
        )
        self.relu2 = nn.ReLU()


        self.pool2 = nn.MaxPool2d(kernel_size=2, stride=2)


        self.fc_input_size = 64 * 1 * 1


        self.fc1 = nn.Linear(self.fc_input_size, 128)
        self.relu3 = nn.ReLU()
        self.dropout = nn.Dropout(0.3)

        self.fc2 = nn.Linear(128, 64)
        self.relu4 = nn.ReLU()

        self.fc3 = nn.Linear(64, 32)
        self.relu5 = nn.ReLU()

        self.fc4 = nn.Linear(32, 1)

    def forward(self, x):

        x = self.conv1(x)
        x = self.relu1(x)
        x = self.pool1(x)

        x = self.conv2(x)
        x = self.relu2(x)
        x = self.pool2(x)


        x = x.view(-1, self.fc_input_size)


        x = self.fc1(x)
        x = self.relu3(x)
        x = self.dropout(x)

        x = self.fc2(x)
        x = self.relu4(x)

        x = self.fc3(x)
        x = self.relu5(x)

        x = self.fc4(x)

        return x



def train_model(model, train_loader, val_loader, criterion, optimizer,
                num_epochs=100, device='cuda'):
    train_losses = []
    val_losses = []

    model.to(device)

    for epoch in range(num_epochs):

        model.train()
        train_loss = 0.0

        for batch_idx, (data, target) in enumerate(train_loader):
            data, target = data.to(device), target.to(device)

            optimizer.zero_grad()
            output = model(data)
            loss = criterion(output, target)
            loss.backward()
            optimizer.step()

            train_loss += loss.item()

        avg_train_loss = train_loss / len(train_loader)
        train_losses.append(avg_train_loss)


        model.eval()
        val_loss = 0.0
        with torch.no_grad():
            for data, target in val_loader:
                data, target = data.to(device), target.to(device)
                output = model(data)
                loss = criterion(output, target)
                val_loss += loss.item()

        avg_val_loss = val_loss / len(val_loader)
        val_losses.append(avg_val_loss)


        if (epoch + 1) % 10 == 0:
            print(f'Epoch [{epoch + 1}/{num_epochs}], '
                  f'Train Loss: {avg_train_loss:.4f}, '
                  f'Val Loss: {avg_val_loss:.4f}')

    return train_losses, val_losses



def evaluate_model(model, test_loader, criterion, device='cuda'):
    model.eval()
    test_loss = 0.0
    predictions = []
    actuals = []

    with torch.no_grad():
        for data, target in test_loader:
            data, target = data.to(device), target.to(device)
            output = model(data)
            loss = criterion(output, target)
            test_loss += loss.item()

            predictions.extend(output.cpu().numpy())
            actuals.extend(target.cpu().numpy())

    avg_test_loss = test_loss / len(test_loader)


    predictions = np.array(predictions).flatten()
    actuals = np.array(actuals).flatten()

    ss_res = np.sum((actuals - predictions) ** 2)
    ss_tot = np.sum((actuals - np.mean(actuals)) ** 2)
    r2 = 1 - (ss_res / ss_tot)


    rmse = np.sqrt(np.mean((actuals - predictions) ** 2))

    return avg_test_loss, r2, rmse, predictions, actuals



def plot_results(train_losses, val_losses, predictions, actuals):
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))


    axes[0].plot(train_losses, label='Train Loss')
    axes[0].plot(val_losses, label='Validation Loss')
    axes[0].set_xlabel('Epoch')
    axes[0].set_ylabel('Loss')
    axes[0].set_title('Training and Validation Loss')
    axes[0].legend()
    axes[0].grid(True)


    axes[1].scatter(actuals, predictions, alpha=0.5)
    axes[1].plot([actuals.min(), actuals.max()],
                 [actuals.min(), actuals.max()], 'r--', lw=2)
    axes[1].set_xlabel('Actual Values')
    axes[1].set_ylabel('Predicted Values')
    axes[1].set_title('Predictions vs Actuals')
    axes[1].grid(True)


    residuals = actuals - predictions
    axes[2].scatter(predictions, residuals, alpha=0.5)
    axes[2].axhline(y=0, color='r', linestyle='--')
    axes[2].set_xlabel('Predicted Values')
    axes[2].set_ylabel('Residuals')
    axes[2].set_title('Residual Plot')
    axes[2].grid(True)

    plt.tight_layout()
    plt.show()



def main():

    CSV_FILE = "Data.csv"
    BATCH_SIZE = 32
    LEARNING_RATE = 0.001
    NUM_EPOCHS = 100
    TEST_SIZE = 0.2
    VAL_SIZE = 0.1


    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")


    print("Loading data...")
    full_dataset = SOCDataset(CSV_FILE)


    train_idx, test_idx = train_test_split(
        range(len(full_dataset)),
        test_size=TEST_SIZE,
        random_state=42
    )

    train_val_idx, val_idx = train_test_split(
        train_idx,
        test_size=VAL_SIZE / (1 - TEST_SIZE),
        random_state=42
    )


    train_dataset = torch.utils.data.Subset(full_dataset, train_val_idx)
    val_dataset = torch.utils.data.Subset(full_dataset, val_idx)
    test_dataset = torch.utils.data.Subset(full_dataset, test_idx)


    train_loader = DataLoader(train_dataset, batch_size=BATCH_SIZE, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=BATCH_SIZE, shuffle=False)
    test_loader = DataLoader(test_dataset, batch_size=BATCH_SIZE, shuffle=False)

    print(f"Training samples: {len(train_dataset)}")
    print(f"Validation samples: {len(val_dataset)}")
    print(f"Testing samples: {len(test_dataset)}")


    print("Initializing model...")
    model = SOC_CNN(input_channels=17)

    criterion = nn.MSELoss()
    optimizer = optim.Adam(model.parameters(), lr=LEARNING_RATE)


    print("Training model...")
    train_losses, val_losses = train_model(
        model, train_loader, val_loader, criterion, optimizer,
        num_epochs=NUM_EPOCHS, device=device
    )


    print("Evaluating model...")
    test_loss, r2, rmse, predictions, actuals = evaluate_model(
        model, test_loader, criterion, device=device
    )

    print(f"\nTest Results:")
    print(f"Test Loss: {test_loss:.4f}")
    print(f"R² Score: {r2:.4f}")
    print(f"RMSE: {rmse:.4f}")


    print("Plotting results...")
    plot_results(train_losses, val_losses, predictions, actuals)


    torch.save({
        'model_state_dict': model.state_dict(),
        'optimizer_state_dict': optimizer.state_dict(),
        'train_losses': train_losses,
        'val_losses': val_losses,
        'r2_score': r2,
        'rmse': rmse
    }, 'soc_cnn_model.pth')

    print("Model saved as 'soc_cnn_model.pth'")


    def predict_single_sample(model, sample_data, device='cuda'):
        """

        Args:
            model:
            sample_data:
            device:
        """
        model.eval()
        with torch.no_grad():

            sample_tensor = torch.FloatTensor(sample_data).unsqueeze(0).to(device)
            prediction = model(sample_tensor)


            prediction_np = prediction.cpu().numpy()
            prediction_original = full_dataset.label_scaler.inverse_transform(
                prediction_np.reshape(-1, 1)
            )

        return prediction_original[0, 0]


    sample_data, _ = test_dataset[0]
    predicted_soc = predict_single_sample(model, sample_data.numpy(), device)
    print(f"\nSample prediction: {predicted_soc:.4f}")



def visualize_model_architecture():

    model = SOC_CNN(input_channels=17)
    print("Model Architecture:")
    print("=" * 50)
    print(model)
    print("=" * 50)


    total_params = sum(p.numel() for p in model.parameters())
    trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)

    print(f"Total parameters: {total_params:,}")
    print(f"Trainable parameters: {trainable_params:,}")


    print("\nLayer Output Shapes:")
    print("-" * 30)


    test_input = torch.randn(1, 17, 9, 9)


    x = test_input
    layer_names = []
    output_shapes = []


    x = model.conv1(x)
    layer_names.append("Conv1")
    output_shapes.append(x.shape)

    x = model.relu1(x)
    layer_names.append("ReLU1")
    output_shapes.append(x.shape)

    x = model.pool1(x)
    layer_names.append("MaxPool1")
    output_shapes.append(x.shape)


    x = model.conv2(x)
    layer_names.append("Conv2")
    output_shapes.append(x.shape)

    x = model.relu2(x)
    layer_names.append("ReLU2")
    output_shapes.append(x.shape)

    x = model.pool2(x)
    layer_names.append("MaxPool2")
    output_shapes.append(x.shape)


    x = x.view(1, -1)
    layer_names.append("Flatten")
    output_shapes.append(x.shape)


    for name, shape in zip(layer_names, output_shapes):
        print(f"{name:15} -> {shape}")


if __name__ == "__main__":

    visualize_model_architecture()


    print("\n" + "=" * 60)
    print("Starting SOC Prediction CNN Training")
    print("=" * 60 + "\n")

    main()