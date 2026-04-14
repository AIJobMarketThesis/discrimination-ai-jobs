import streamlit as st
import os
from utils import get_openai_client, batch_generate, answer_screening_question

# Configuration
st.set_page_config(page_title="Resume & Cover Letter Generator", layout="wide")

# Title and Intro
st.title("Synthetic Application Generator")
st.markdown("""
Generate **bulk professional resumes and cover letters** tailored to a specific job description.
Each candidate receives a unique visual theme and personalized content.
""")

# Sidebar settings
st.sidebar.header("Configuration")
# API Key is expected to be in .env or environment variables
if not os.getenv("OPENAI_API_KEY"):
    st.warning("⚠️ OPENAI_API_KEY not found in environment. Please set it in .env file.")

target_country = st.sidebar.selectbox("Target Country", ["Germany", "France", "Italy", "Netherlands", "Finland"])
num_candidates = st.sidebar.number_input("Number of Candidates", min_value=2, max_value=6, value=6, step=1)
education_level = st.sidebar.selectbox("Education Level", ["Master of Science in Computer Science", "Bachelor of Science in Computer Science"])
# Main UI
st.subheader("1. Job Description")
job_description = st.text_area(
    "Paste the Job Description here...",
    height=300,
    placeholder="e.g. Seeking a Senior Python Developer with experience in Django, AWS..."
)

st.subheader("2. Generation")
generate_btn = st.button("🚀 Generate Resumes", type="primary")

if generate_btn:
    client = get_openai_client()
    if not client:
        st.error("❌ OpenAI API Key is missing. Please set provided input or environment variable.")
    elif not job_description.strip():
        st.error("❌ Please enter a Job Description.")
    else:
        with st.spinner(f"Creating {num_candidates} detailed candidate profiles for {target_country}... This may take a minute."):
            try:
                zip_file, candidates_data, zip_filename = batch_generate(
                    job_description, 
                    int(num_candidates), 
                    education_level, 
                    target_country, 
                    client
                )
                if zip_file:
                    st.session_state['generated_zip'] = zip_file
                    st.session_state['candidates_data'] = candidates_data
                    st.session_state['job_description'] = job_description  # Store for chatbot
                    st.session_state['zip_filename'] = zip_filename
                    st.success(f"✅ Generation Complete! Created {len(candidates_data)} candidates for {target_country}.")
                    
                    # Show quick preview of generated candidates
                    st.subheader("Generated Candidates:")
                    preview_cols = st.columns(min(len(candidates_data), 3))
                    for idx, (cand, _, _, _) in enumerate(candidates_data[:3]):  # Show first 3
                        with preview_cols[idx % 3]:
                            st.info(f"**{cand['name']}**\n\n📍 {cand['location']}\n🎓 {cand.get('university', 'University')}")
                else:
                    st.error("Failed to generate candidates. Please check API quota or try again.")
            except Exception as e:
                st.error(f"An error occurred: {str(e)}")

# Display Download Button if data exists in session state
if 'generated_zip' in st.session_state:
    st.download_button(
        label="📥 Download All Resumes & Cover Letters (ZIP)",
        data=st.session_state['generated_zip'],
        file_name=st.session_state.get('zip_filename', "applications.zip"),
        mime="application/zip"
    )

# Application Assistant Chatbot
if 'candidates_data' in st.session_state:
    st.markdown("---")
    st.subheader("3. Application Assistant")
    st.write("Answer screening questions (e.g., from Indeed) acting as one of the generated candidates.")
    
    candidates = st.session_state['candidates_data']
    candidate_names = [c[0]['name'] for c in candidates]
    
    selected_name = st.selectbox("Select Candidate Persona", candidate_names)
    selected_candidate_tuple = next(c for c in candidates if c[0]['name'] == selected_name)
    cand_dict, resume_data, _, _ = selected_candidate_tuple
    
    # Show selected candidate context
    with st.expander(f"📋 {selected_name} - Profile Context"):
        st.write(f"**Location:** {cand_dict['location']}")
        st.write(f"**University:** {cand_dict.get('university', 'N/A')}")
        if 'email' in cand_dict:
            st.write(f"**Email:** {cand_dict['email']}")
        if 'phone' in cand_dict:
            st.write(f"**Phone:** {cand_dict['phone']}")
    
    question = st.text_area("Screening Question", placeholder="e.g., How many years of experience do you have in Python?")
    
    col1, col2 = st.columns([1, 5])
    with col1:
        ask_btn = st.button("🤔 Generate Answer")
    
    if ask_btn and question.strip():
        client = get_openai_client()
        with st.spinner(f"Generating answer as {selected_name}..."):
            answer = answer_screening_question(
                cand_dict, 
                resume_data, 
                st.session_state.get('job_description', ''), 
                question, 
                client
            )
            st.markdown("**Answer:**")
            st.info(answer)
    elif ask_btn and not question.strip():
        st.warning("Please enter a question.")

# Footer
st.markdown("---")
st.caption("Powered by OpenAI & ReportLab. Developed for synthetic data generation.")
st.caption(f"📍 Locations and universities are dynamically generated based on the selected country: {target_country if 'target_country' in locals() else 'Not selected'}")
