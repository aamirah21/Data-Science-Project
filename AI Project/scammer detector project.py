import cv2
from pyzbar.pyzbar import decode
import nltk
import re
import requests
from bs4 import BeautifulSoup
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.naive_bayes import MultinomialNB
import tkinter as tk
from tkinter import messagebox, Tk, Label, Toplevel, Message
from PIL import Image, ImageTk, ImageEnhance
import threading



# Download NLTK resources
nltk.download('punkt')
nltk.download('stopwords')
nltk.download('wordnet')


class ScamDetector:
    def __init__(self):
        self.vectorizer = TfidfVectorizer(max_features=1000)
        self.classifier = MultinomialNB()
        self.stop_words = set(stopwords.words("english"))
        self.lemmatizer = WordNetLemmatizer()
        self.known_scam_numbers = [
            '1234567890',
            '5555555555',
            '8005551212',
            '9001234567',
            '8761234567',
            '8001234567',
            '8441234567',
            '8551234567',
            '8661234567',
            '8771234567',
            '8881234567',
            '8091234567',
            '9002345678',
            '8009876543',
            '8449876543',
            '8559876543',
            '8669876543',
            '8779876543',
            '8889876543',
            '8099876543',
            '60169494016',
            '60105687366',
            '60199848088',
            '601137788921',
            '60107150445',
            '6285314112053',
            '60164307673',
            '6282126866465',
            '60163489426',
            '60182252043',
        ]

    def preprocess_text(self, text):
        tokens = word_tokenize(text.lower())
        tokens = [self.lemmatizer.lemmatize(token) for token in tokens if token.isalnum()]
        tokens = [token for token in tokens if token not in self.stop_words]
        return " ".join(tokens)

    def train(self, data):
        X = [self.preprocess_text(text) for text, label in data]
        y = [label for text, label in data]
        X_tfidf = self.vectorizer.fit_transform(X)
        self.classifier.fit(X_tfidf, y)

    def extract_text_from_url(self, url):
        try:
            response = requests.get(url, timeout=5)
            soup = BeautifulSoup(response.content, 'html.parser')
            text = ' '.join([p.get_text() for p in soup.find_all('p')])
            return text
        except requests.exceptions.RequestException as e:
            return str(e)

    def detect_scam(self, message):
        # Check for the specific word 'kaya'
        if 'kaya' in message.lower():
            return 'scam'

        processed_message = self.preprocess_text(message)
        vectorized_message = self.vectorizer.transform([processed_message])
        prediction = self.classifier.predict(vectorized_message)
        return prediction[0]

    def detect_scam_from_website(self, url):
        if 'kaya' in url.lower():
            return 'scam'
        text = self.extract_text_from_url(url)
        if text:
            return self.detect_scam(text)
        return 'legitimate'

    def detect_phone_number_scam(self, text):
        phone_numbers = re.findall(r'\b\d{10,12}\b', text)  # Simple regex for finding 10-12 digit phone numbers
        for number in phone_numbers:
            if number in self.known_scam_numbers:
                return 'scam'
        return 'legitimate'

    def is_legitimate_url(self, url):
        if url.startswith("https://") or url.startswith("🔒https://"):  # Check for 'https://' or '🔒https://'
            return True
        else:
            return False

    def decode_and_verify_qr_codes(self, frame):
        # Convert the frame to grayscale
        gray_frame = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

        # Decode QR codes in the frame
        decoded_objects = decode(gray_frame)

        # Iterate over all detected QR codes and verify against the trusted criteria
        for obj in decoded_objects:
            # Extract QR code data
            qr_data = obj.data.decode('utf-8')

            print("Detected QR code:", qr_data)  # Print the detected QR code for debugging

            # Check if the QR code content is a legitimate URL
            if self.is_legitimate_url(qr_data):
                result = "Legitimate"
                color = (0, 255, 0)  # Green for legitimate
            else:
                result = "Scammer"
                color = (0, 0, 255)  # Red for scammer

            # Draw a rectangle around the QR code
            (x, y, w, h) = obj.rect
            cv2.rectangle(frame, (x, y), (x + w, y + h), color, 2)

            # Put the result on the frame
            cv2.putText(frame, result, (x, y - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, color, 2)

        return frame


# Initialize the ScamDetector and train it with sample data
detector = ScamDetector()
scam_data = [
    ("Click this link to win a free iPhone!", "scam"),
    ("Invest in this amazing opportunity for guaranteed returns!", "scam"),
    ("Get rich quick by working from home!", "scam"),
    ("Your account has been compromised. Click here to reset your password.", "scam"),
    ("Join our network and earn money by recruiting others.", "scam"),
    ("Check out this cute cat video!", "legitimate"),
    ("Happy birthday to our dear friend!", "legitimate"),
    ("Congratulations on your new job!", "legitimate"),
    ("Here's a discount code for your next purchase.", "legitimate"),
    ("Reminder: Don't forget to backup your data regularly.", "legitimate")
]
detector.train(scam_data)


def detect_link_scam():
    url = input_field.get()
    if url.startswith('http'):
        result = detector.detect_scam_from_website(url)
        if result == 'scam':
            messagebox.showinfo("Link Scam Detection", "This URL appears to be a scam.")
        else:
            messagebox.showinfo("Link Scam Detection", "This URL appears to be legitimate. Please be careful.")
    else:
        messagebox.showwarning("Invalid Input", "Please enter a valid URL.")
    input_field.delete(0, tk.END)  # Clear the input field


def detect_message_scam():
    message = input_field.get()
    result = detector.detect_scam(message)
    if result == 'scam':
        messagebox.showinfo("Message Scam Detection", "This message appears to be a scam.")
    else:
        messagebox.showinfo("Message Scam Detection", "This message appears to be legitimate. Please be careful.")
    input_field.delete(0, tk.END)  # Clear the input field


def detect_phone_number():
    message = input_field.get()
    # Show a message about the phone number format
    messagebox.showinfo("Phone Number Format",
                        "For Malaysian numbers, please include the country code '60'. Example: 60123456789")

    # Find all numbers with at least 11 digits starting with '60'
    phone_numbers = re.findall(r'\b60\d{9,11}\b', message)
    print("Detected phone numbers:", phone_numbers)

    if not phone_numbers:
        messagebox.showwarning("Invalid Input", "No valid phone numbers detected. Ensure numbers start with '60' and are at least 11 digits long.")
        return

    # Check for invalid phone numbers
    for number in phone_numbers:
        if len(number) < 11:
            print("Invalid Malaysian Phone Number:", number)
            messagebox.showwarning("Invalid Malaysian Phone Number",
                                   "Invalid Malaysian phone number. It must be at least 11 digits long.")
            input_field.delete(0, tk.END)
            return

    result = detector.detect_phone_number_scam(message)
    if result == 'scam':
        messagebox.showinfo("Phone Number Scam Detection",
                            "This phone number appears to be associated with a scam. Please be careful.")
    else:
        messagebox.showinfo("Phone Number Scam Detection",
                            "This phone number appears to be legitimate.")
    input_field.delete(0, tk.END)  # Clear the input field


def exit_application():
    # Function to exit the application
    root.destroy()


def run_qr_scanner():
    # Access the default camera (0)
    cap = cv2.VideoCapture(0)

    while True:
        # Capture frame-by-frame
        ret, frame = cap.read()

        # Check if the frame was captured successfully
        if not ret:
            print("Error: Failed to capture frame")
            break

        # Decode and verify QR codes in the frame
        frame_with_verification_result = detector.decode_and_verify_qr_codes(frame)

        # Add instructions to exit the QR scanner
        cv2.putText(frame_with_verification_result, "Press 'q' to exit QR Scanner", (10, 30), cv2.FONT_HERSHEY_SIMPLEX,
                    0.7, (0, 255, 255), 2)

        # Show the frame with the exit instructions
        cv2.imshow('QR Code Scanner', frame_with_verification_result)

        # Check for key press, if 'q' is pressed, exit the loop
        if cv2.waitKey(1) & 0xFF == ord('q'):
            break

    # Release the camera and close OpenCV windows
    cap.release()
    cv2.destroyAllWindows()


def start_qr_scanner():
    qr_thread = threading.Thread(target=run_qr_scanner)
    qr_thread.start()


def show_main_frame():
    welcome_frame.pack_forget()
    main_frame.pack(fill='both', expand=True)


# Create the main window
root = tk.Tk()
root.title("Scammer Detector")
root.geometry("800x600")

# Load and configure the background image
background_image_path = R"C:\Users\User\Downloads\phone-Photoroom (2).jpg"
background_image = Image.open(background_image_path)
background_image = background_image.resize((root.winfo_screenwidth(), root.winfo_screenheight()), Image.LANCZOS)
background_photo = ImageTk.PhotoImage(background_image)

# Enhance the sharpness of the image
enhancer = ImageEnhance.Sharpness(background_image)
background_image = enhancer.enhance(5.0)  # Increase the factor for more sharpness, e.g., 2.0 or 3.0

# Resize the image to fit the screen
background_image = background_image.resize((root.winfo_screenwidth(), root.winfo_screenheight()), Image.LANCZOS)

# Convert the image to a PhotoImage object for Tkinter
background_photo = ImageTk.PhotoImage(background_image)

# Create a label widget to display the background image
background_label = Label(root, image=background_photo)
background_label.place(x=0, y=0, relwidth=1, relheight=1)



# Create the welcome frame
welcome_frame = tk.Frame(root)
welcome_frame.pack(fill='both', expand=True)

# Display the background image in the welcome frame
background_label = tk.Label(welcome_frame, image=background_photo)
background_label.place(x=0, y=0, relwidth=1, relheight=1)

# Create the main application frame but do not pack it yet
main_frame = tk.Frame(root)

# Display the background image in the main frame
background_label_main = tk.Label(main_frame, image=background_photo)
background_label_main.place(x=0, y=0, relwidth=1, relheight=1)

# Load the icon image
icon_image = Image.open("C:\\Users\\User\\Desktop\\UMPSA 23_24\\artificial intelligence 23_24\\GROUP ASSIGNMENT AI\\SCAM ALERT.jpg")
icon_image = icon_image.resize((500, 250), Image.LANCZOS)
icon_photo = ImageTk.PhotoImage(icon_image)

# Create a label for the icon
icon_label = tk.Label(welcome_frame, image=icon_photo, bg="#800000")  # Maroon background color
icon_label.pack(pady=90)

# Create a welcome message label with custom font
welcome_font = ("Times New Roman", 35, "bold")
welcome_label = tk.Label(welcome_frame, text="Welcome to Scammer Detector", font=welcome_font, bg="#800020",
                         fg="white")
welcome_label.pack(pady=10)

# Create a container frame for the continue button to provide better alignment
button_frame = tk.Frame(welcome_frame, bg="#800000")  # Maroon background color
button_frame.pack()

# Create a button to proceed to the main application
continue_button = tk.Button(welcome_frame, text="Continue", command=show_main_frame,
                            font=("Times New Roman", 17, "bold"), bg="maroon", fg="white", bd=5, relief=tk.RAISED,
                            height=1, width=20)
continue_button.pack(pady=10)

# Pack the button frame to the bottom of the welcome frame
button_frame.pack(side=tk.BOTTOM, pady=(0, 30))

# Create a label for the title with custom font
title_font = ("Times New Roman", 60, "bold")
title_label = tk.Label(main_frame, text="Scammer Detector", font=title_font, bg="maroon", fg="white")
title_label.pack(pady=150)

# Create an entry field for user input
input_field = tk.Entry(main_frame, width=50, font=("Times New Roman", 14), bd=5, relief=tk.GROOVE)
input_field.pack(pady=5)

# Create buttons for detecting link URL, message scammers, phone scammer, & QR scammer with custom font and size
button_font = ("Times New Roman", 16, "bold")
button_style = {"font": button_font, "bg": "#800020", "fg": "white", "bd": 5, "relief": tk.RAISED, "height": 1,
                "width": 20}

link_button = tk.Button(main_frame, text="Detect Link URL", command=detect_link_scam, **button_style)
link_button.pack(pady=10)

message_button = tk.Button(main_frame, text="Detect Message", command=detect_message_scam, **button_style)
message_button.pack(pady=10)

phone_button = tk.Button(main_frame, text="Detect Phone Number", command=detect_phone_number, **button_style)
phone_button.pack(pady=10)

qr_button = tk.Button(main_frame, text="QR Scanner", command=start_qr_scanner, **button_style)
qr_button.pack(pady=10)

# Create an "Exit" button
exit_button = tk.Button(main_frame, text="Exit", command=exit_application, **button_style)
exit_button.pack(pady=10)

# Run the application
root.mainloop()