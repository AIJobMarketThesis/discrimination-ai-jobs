

import os
import json
import zipfile
import io
import random
import concurrent.futures
from typing import List, Dict, Tuple
import openai
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, HRFlowable
from reportlab.lib import colors
from datetime import datetime
import re

def clean_text_formatting(text: str) -> str:
    """
    Converts Markdown-style formatting (*, **, _) to ReportLab-compatible XML tags.
    """
    if not isinstance(text, str):
        return str(text)
        
    # Bold: **text** -> <b>text</b>
    text = re.sub(r'\*\*(.*?)\*\*', r'<b>\1</b>', text)
    
    # Italics: *text* or _text_ -> <i>text</i>
    text = re.sub(r'\*(.*?)\*', r'<i>\1</i>', text)
    text = re.sub(r'_(.*?)_', r'<i>\1</i>', text)
    
    return text

# Load environment variables if running locally
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    pass

def get_openai_client():
    api_key = os.getenv("OPENAI_API_KEY")
    # Streamlit Cloud deployment often uses st.secrets, but for now we rely on env vars or user input if implemented.
    if not api_key:
        return None
    return openai.OpenAI(api_key=api_key)

# REMOVED location from static candidates - now only contains name, gender, origin, email, phone
STATIC_CANDIDATES =  [
  {
    "name": "Jonas Schneider",
    "gender": "Male",
    "origin": "White",
    "email": "jonasschneid1@outlook.com",
    "phone": "+49 15510 118262"
  },
  {
    "name": "Alexander Meyer",
    "gender": "Male",
    "origin": "White",
    "email": "m.alexander2303@outlook.com",
    "phone": "+49 1523 8425990"
  },
  {
    "name": "Niklas Schmidt",
    "gender": "Male",
    "origin": "White",
    "email": "nikschmidt22@hotmail.com",
    "phone": "+49 163 2921406"
  },
  {
    "name": "Marie Fischer",
    "gender": "Female",
    "origin": "White",
    "email": "marie-fischerr@outlook.com",
    "phone": "+49 1575 4611441"
  },
  {
    "name": "Julia Weber",
    "gender": "Female",
    "origin": "White",
    "email": "juliaweber00@hotmail.com",
    "phone": "+49 176 25402552"
  },
  {
    "name": "Katharina Müller",
    "gender": "Female",
    "origin": "White",
    "email": "kath.muller@outlook.com",
    "phone": "+49 1525 1769788"
  },
  {
    "name": "Arjun Patel",
    "gender": "Male",
    "origin": "Asian",
    "email": "patel.arj@outlook.com",
    "phone": "+49 1522 6704107"
  },
  {
    "name": "Rahul Sharma",
    "gender": "Male",
    "origin": "Asian",
    "email": "sharma.rahul33@outlook.com",
    "phone": "+49 1517 2332314"
  },
  {
    "name": "Min Jun Jeong",
    "gender": "Male",
    "origin": "Asian",
    "email": "jeong.minjun@outlook.com",
    "phone": "+49 1522 7989642"
  },
  {
    "name": "Priya Kumar",
    "gender": "Female",
    "origin": "Asian",
    "email": "priyakumar81@outlook.com",
    "phone": "+49 1521 6421918"
  },
  {
    "name": "Seoyeon Kim",
    "gender": "Female",
    "origin": "Asian",
    "email": "kim.seoyeon7@outlook.com",
    "phone": "+49 176 43881053"
  },
  {
    "name": "Minseo Choi",
    "gender": "Female",
    "origin": "Asian",
    "email": "choi.min-seo@outlook.com",
    "phone": "+49 176 74525750"
  },
  {
    "name": "Santiago Ramirez",
    "gender": "Male",
    "origin": "African",
    "email": "santis.ramirez@outlook.com",
    "phone": "+49 176 60874514"
  },
  {
    "name": "Matheus Pereira",
    "gender": "Male",
    "origin": "Latin American",
    "email": "mathe.pereira@outlook.com",
    "phone": "+49 15510 624789"
  },
  {
    "name": "Emmanuel Adebayo",
    "gender": "Male",
    "origin": "Latin American",
    "email": "emmanuel.adeb.work@outlook.com",
    "phone": "+49 162 6664232"
  },
  {
    "name": "Adriana Ferreira",
    "gender": "Female",
    "origin": "African",
    "email": "adriana.ferreira.work@outlook.com",
    "phone": "+49 176 76329485"
  },
  {
    "name": "Maria Fernanda Reyes",
    "gender": "Female",
    "origin": "Latin American",
    "email": "maria.fern.reyes@outlook.com",
    "phone": "+49 162 1529823"
  },
  {
    "name": "Chioma Okafor",
    "gender": "Female",
    "origin": "Latin American",
    "email": "chioma.okafor4@outlook.com",
    "phone": "+49 174 6723390"
  }

]

COMPANY_POOL = [
    "SAP", "Siemens", "Allianz", "BMW", "Mercedes-Benz", "Volkswagen", "Deutsche Bank", 
    "Adidas", "Puma", "BASF", "Bayer", "Merck", "Deutsche Telekom", "E.ON", "Infineon", 
    "Zalando", "HelloFresh", "N26", "Delivery Hero", "Personio", "Celonis", "Trade Republic", 
    "Gorillas", "Flink", "Auto1", "Trivago", "Xing", "Soundcloud", "Babbel", "Tier", 
    "Voi", "Lilium", "Volocopter", "FlixBus", "Lufthansa", "Airbus", "Bosch", 
    "Continental", "Henkel", "Beiersdorf", "Audi", "Porsche", "DHL", "Commerzbank",
    "Munich Re", "RWE", "ThyssenKrupp", "HeidelbergCement", "Fresenius", "Covestro",
    "HCL Technologies", "Infosys", "Wipro", "Capgemini", "Accenture", "TCS"
]

UNIVERSITY_POOL = [
    "Technical University of Munich", "Technische Universität Berlin", 
    "Ludwig-Maximilians-Universität München", "Karlsruhe Institute of Technology", 
    "RWTH Aachen University", "Humboldt-Universität zu Berlin", 
    "Universität Heidelberg", "Technical University of Darmstadt", 
    "Universität Stuttgart", "Technische Universität Dresden", 
    "Friedrich-Alexander-Universität Erlangen-Nürnberg", "Leibniz University Hannover"
]

COUNTRY_DATA = {
    "Germany": {
        "cities": ["Berlin", "Munich", "Hamburg", "Frankfurt", "Stuttgart", "Cologne", "Düsseldorf"],
        "universities": ["Technical University of Munich", "Technische Universität Berlin", "Ludwig-Maximilians-Universität München", "Karlsruhe Institute of Technology", "RWTH Aachen University", "Humboldt-Universität zu Berlin", "Universität Heidelberg", "Technical University of Darmstadt", "Universität Stuttgart", "Technische Universität Dresden", "Friedrich-Alexander-Universität Erlangen-Nürnberg", "Leibniz University Hannover"],
        "companies": [
            "SAP", "Siemens", "BMW", "Allianz", "Volkswagen", "Mercedes-Benz", "Deutsche Telekom", 
            "BASF", "Bayer", "Munich Re", "Deutsche Bank", "Porsche", "Adidas", "Bosch", 
            "DHL", "Continental", "E.ON", "Infineon", "Henkel", "ThyssenKrupp"
        ],
        "language": "German"
    },
    "France": {
        "cities": ["Paris", "Lyon", "Marseille", "Toulouse", "Nice", "Bordeaux", "Lille"],
        "universities": ["Sorbonne University", "Ecole Polytechnique", "Université PSL", "Telecom Paris", "CentraleSupélec", "HEC Paris", "ESSEC Business School"],
        "companies": [
            "L'Oréal", "TotalEnergies", "Sanofi", "AXA", "BNP Paribas", "Capgemini", "Dassault Systèmes", 
            "Orange", "LVMH", "Airbus", "Michelin", "Renault", "Danone", "Kering", 
            "Schneider Electric", "Société Générale", "Crédit Agricole", "Engie", "Carrefour", "Vinci"
        ],
        "language": "French"
    },
    "Italy": {
        "cities": ["Rome", "Milan", "Naples", "Turin", "Florence", "Bologna", "Venice"],
        "universities": ["Politecnico di Milano", "Sapienza University of Rome", "University of Bologna", "University of Padua", "Politecnico di Torino", "Bocconi University"],
        "companies": [
            "Enel", "Eni", "Stellantis", "Intesa Sanpaolo", "Leonardo", "Pirelli", "Ferrari", "Prada", 
            "UniCredit", "Telecom Italia", "Generali", "Poste Italiane", "Ferrero", "Barilla", 
            "Luxottica", "Campari", "Snam", "Terna", "Prysmian", "Armani"
        ],
        "language": "Italian"
    },
    "Netherlands": {
        "cities": ["Amsterdam", "Rotterdam", "The Hague", "Utrecht", "Eindhoven", "Groningen", "Maastricht"],
        "universities": ["Delft University of Technology", "University of Amsterdam", "Eindhoven University of Technology", "Leiden University", "Utrecht University", "Erasmus University Rotterdam"],
        "companies": [
            "ASML", "Philips", "Shell", "Unilever", "Heineken", "Adyen", "Booking.com", "ING", 
            "Ahold Delhaize", "KPN", "NXP Semiconductors", "Randstad", "Aegon", "AkzoNobel", 
            "Rabobank", "DSM-Firmenich", "Just Eat Takeaway", "NN Group", "TomTom", "KLM"
        ],
        "language": "Dutch"
    },
    "Finland": {
        "cities": ["Helsinki", "Espoo", "Tampere", "Vantaa", "Oulu", "Turku", "Jyväskylä"],
        "universities": ["University of Helsinki", "Aalto University", "Tampere University", "University of Oulu", "University of Turku", "LUT University"],
        "companies": [
            "Nokia", "Kone", "Wärtsilä", "Neste", "Stora Enso", "Supercell", "Rovio", "Tietoevry", 
            "UPM-Kymmene", "Sampo Group", "Nordea", "Fortum", "Kesko", "Valmet", 
            "Huhtamäki", "Outokumpu", "Cargotec", "Fazer", "Orion", "Kemira"
        ],
        "language": "Finnish"
    }
}

def get_random_theme():
    """Generates a random design theme for Resume and Cover Letter."""
    colors_list = [
        colors.darkblue, colors.darkslategray, colors.black, colors.darkgreen, 
        colors.darkred, colors.navy, colors.midnightblue, colors.teal,
        colors.firebrick, colors.seagreen, colors.indigo, colors.dimgray,
        colors.brown, colors.purple, colors.darkorange
    ]
    fonts_list = ['Helvetica-Bold', 'Times-Bold', 'Courier-Bold']
    
    return {
        'resume_header_color': random.choice(colors_list),
        'resume_company_style': random.choice(['Italic', 'Bold']),
        'resume_name_font': random.choice(['Helvetica-Bold', 'Times-Bold']),
        'resume_name_alignment': random.choice([0, 1]), # 0=Left, 1=Center
        'resume_header_case': random.choice(['UPPER', 'Title']), # UPPER or Title Case
        'cl_header_alignment': random.choice([0, 1]), # 0=Left, 1=Center
        'cl_header_font': random.choice(fonts_list),
        'cl_header_color': random.choice(colors_list)
    }

def generate_demographic_data(n: int, client) -> List[Dict]:
    candidates = STATIC_CANDIDATES

    females = [c for c in candidates if c['gender'] == 'Female']
    males = [c for c in candidates if c['gender'] == 'Male']

    def get_by_origin(pool, target_origins, limit):
        matches = [c for c in pool if c['origin'] in target_origins]
        return random.sample(matches, min(limit, len(matches)))

    selected = []

    # Females
    selected.extend(get_by_origin(females, ['Asian'], 1))
    selected.extend(get_by_origin(females, ['White'], 1))
    selected.extend(get_by_origin(
        females,
        [ 'African', 'Latin American'],
        1
    ))

    # Males
    selected.extend(get_by_origin(males, ['Asian'], 1))
    selected.extend(get_by_origin(males, ['White'], 1))
    selected.extend(get_by_origin(
        males,
        [ 'African', 'Latin American'],
        1
    ))

    # Fallback if needed
    if len(selected) < 6:
        remaining = [c for c in candidates if c not in selected]
        needed = 6 - len(selected)
        selected.extend(random.sample(remaining, min(needed, len(remaining))))

    return selected

def generate_resume_content(candidate: Dict, job_description: str, education_level: str, target_country: str, client) -> Dict:
    # Current date context
    current_date_obj = datetime.now()
    current_month_year = current_date_obj.strftime("%B %Y")
    current_year = current_date_obj.year

    # Language Rule: Check if target language is explicitly mentioned in JD (case-insensitive)
    target_lang = COUNTRY_DATA[target_country]["language"]
    
    if target_country == "Germany":
        language_instruction = f"7. Languages: Include **German (C1)** and **English (C1)**."
    else:
        include_lang = target_lang.lower() in job_description.lower()
        if include_lang:
            language_instruction = f"7. Languages: Include **{target_lang} (C1)** and **English (C1/Fluent)**."
        else:
            language_instruction = f"7. Languages: DO NOT include {target_lang}. Only include English and other relevant languages if applicable (but NO {target_lang} unless requested in JD)."

    # Company injection
    base_companies = random.sample(COMPANY_POOL, 4)
    country_companies = random.sample(COUNTRY_DATA[target_country]["companies"], min(4, len(COUNTRY_DATA[target_country]["companies"])))
    suggested_companies = ", ".join(country_companies + base_companies)

    # Origin context for diversity-aware generation
    origin_context = candidate.get('origin', 'General')
    
    # Get location from target country
    candidate_city = random.choice(COUNTRY_DATA[target_country]["cities"])
    candidate_location = f"{candidate_city}, {target_country}"
    
    education_json_template = ""
    if "Master" in education_level:
        education_json_template = f"""[
        {{
            "degree": "Master of Science in Computer Science",
            "university": "{candidate.get('masters_university', 'University')}",
            "year": "YYYY",
            "details": "Relevant coursework or specialization"
        }},
        {{
            "degree": "Bachelor of Science in Computer Science",
            "university": "{candidate.get('bachelors_university', 'University')}",
            "year": "YYYY",
            "details": "..."
        }}
    ]"""
    else:
        education_json_template = f"""[
        {{
            "degree": "Bachelor of Science in Computer Science",
            "university": "{candidate.get('bachelors_university', 'University')}",
            "year": "YYYY",
            "details": "Relevant coursework or specialization"
        }}
    ]"""

    prompt = f"""
    You are an expert ATS-optimized resume writer specializing in the job market.
    Generate professional, dense, and high-quality resume content for {candidate['name']} aiming for a one-page professional standard.
    
    Candidate Context:
    - Location: {candidate_location}
    - Dynamic Context (Origin): {origin_context}. (Use to subtly inform style if relevant, but keep professional).
    - Current Date: {current_month_year}
    - Education: {education_level}
    
    Education Strictness Rules:
    - **IF {education_level} IS "Bachelor of Science in Computer Science"**:
        - Include ONLY this Bachelor's degree in the education section. 
        - DO NOT include any Master's degree.
        - University: Use ONLY {candidate.get('bachelors_university', 'a university')}.
    - **IF {education_level} IS "Master of Science in Computer Science"**:
        - You MUST include TWO education entries:
            1. **Master of Science in Computer Science** from {candidate.get('masters_university', 'a university')}.
            2. **Bachelor of Science in Computer Science** from {candidate.get('bachelors_university', 'a university')}.
        - Ensure the years for these degrees are logically spaced (e.g., Bachelor's completed 2 years before Master's).
    
    Experience & Role Identification:
    1. Identify the **Target Role** (e.g., "Data Engineer", "Technical Lead") directly from the Job Description.
    2. Analyze the Job Description to identify required years of experience (e.g., "3 years of experience", "Senior level").
    3. Calculate the "Graduation Year" as: {current_year} - (Required Experience Years).
    4. If no specific years are mentioned, assume 2 years for a Master's and 0-1 years for a Bachelor's.
    5. **STRICT COUNT - Experience**: You MUST generate EXACTLY 4 professional experience entries sum to the exact duration of experience required by the job description.
    6. **EXPERIENCE SPLIT**:
        - If JD requires 0-1 years: Use 1 Full-time/Junior role and 2-3 Internships/Working Student roles.
        - If JD requires 2-4 years: Use 2 Full-time roles and 1-2 Internships.
        - If JD requires 5+ years: Use 3-4 Full-time roles and 0-1 Internship.
    7. **STRICT COUNT - Projects**: You MUST generate EXACTLY 4 key projects.
    8. Graduation Year should be the year the candidate completed the {education_level}. 
    9. For senior roles, include specific roles like "Technical Lead" or "Senior [Role]" at companies from the pool.
    10. **DATE FORMATTING**: All durations/dates in the experience section MUST use full month names (e.g., "June 2022 - August 2024"). DO NOT use month numbers (e.g., "06/2022").
    11. **STRICT DATE RULE**: DO NOT generate any dates in the future (after {current_month_year}).
        - If a job is ongoing, use "Present" as the end date (e.g., "January 2024 - Present").
        - If a job starts in {current_year}, it MUST NOT end after {current_month_year} unless it is "Present".
        - Example: "January {current_year} - Present" is valid. "August {current_year}" is INVALID.
    12. **CHRONOLOGICAL ROLE RULES**:
        - "Working Student" roles MUST follow this STRICT rule:

- The role MUST end EXACTLY in **December of the year immediately before the Graduation Year**.

Example:
If Graduation Year = 2024
Working Student end date MUST be:
"December 2023"

Valid:
"March 2023 - December 2023"
"July 2022 - December 2023"

INVALID:
"March 2023 - August 2023"
"March 2023 - November 2023"
"January 2024 - May 2024"

Working Student roles MUST NEVER extend into the Graduation Year.
        - "Full-time" or "Junior" roles MUST occur **after** the Graduation Year. 
        - The **most recent role** MUST be a Full-time/Junior role (not an internship) and MUST end in "**Present**" (unless the candidate is a very fresh grad with 0 months experience).
    13. **NO GAP RULE**: Ensure there are **ABSOLUTELY NO gaps** in the work experience timeline. 
        - The end date of one role should be the same month or the month immediately preceding the start date of the next role.
        - Sequence the roles logically back-to-back leading to the current date.
    14. **NO OVERLAPPING DATES**: Ensure there are **NO overlapping dates** in the work experience timeline. 
        - One job must end before or in the same month that the next job starts.
        - Two full-time jobs cannot occur at the same time.
        - A "Working Student" role cannot overlap with a "Full-time" role.
    
    Job Description:
    {job_description}
    
    Strict Writing Rules:
    1. **REAL COMPANIES ONLY**: All companies listed in experience MUST be real, existing companies.
       - Use companies of a **similar market level/ranking**.
       - Suggested companies to draw from (or use similar): {suggested_companies}.
       - Do NOT use fictional names like "TechSolutions Inc". Use real names relevant to the location/industry.
       - STRICT RULE: The candidate's past and current companies MUST NOT be the same company they are applying to (the company mentioned in the Job Description). Ensure they are distinct.
    2. **CONSISTENT EXPERIENCE LEVEL**: Experience level/seniority must be adjusted to the JD requirements.
    3. **ATS Optimization**: Keywords must match the JD.
    4. Each **internship/work experience** must contain **3–4 achievement-focused bullet points**.
    5. Each **project** must be HIGHLY RELEVANT to the Job Description with a **3–4 line technical description**.
    6. Maintain a concise **one-page density**.
    {language_instruction}
    8. **Sections**: Contact, Summary, Education, Experience, Projects, Skills, Certificates, Languages.
    PROJECT TITLE PROFESSIONALISM RULES (MANDATORY):

9. Project titles MUST be written in a PROFESSIONAL, resume-appropriate format.
10. Titles MUST sound like real engineering or enterprise projects and NOT like tutorials or coursework.

STRICTLY AVOID titles such as:
- "RAG Implementation"
- "Building CI/CD Pipeline"
- "Chatbot Project"
- "Machine Learning Model"
- "Data Analysis Project"

INSTEAD, use professional titles such as:
- "Enterprise AI Knowledge Retrieval Platform"
- "Scalable Customer Intelligence Data Pipeline"
- "Automated Fraud Detection System"
- "Cloud-Native Microservices Deployment Architecture"
- "Real-Time Financial Risk Prediction Engine"
- "AI-Powered Document Intelligence System"

11. Titles MUST follow one of these professional structures:
   - [Business Problem] + [System / Platform]
   - [Domain] + [Analytics / Intelligence System]
   - [Enterprise / Scalable / Automated] + [Platform / Engine / Framework]

Examples of acceptable formats:
- "Enterprise Document Intelligence Platform"
- "Scalable Real-Time Fraud Detection System"
- "AI-Powered Customer Insights Platform"
- "Distributed Data Processing Framework for Financial Analytics"

4. Each project MUST appear like a real production-grade or enterprise engineering project relevant to the Job Description.

5. Project descriptions MUST focus on:
   - architecture
   - scalability
   - performance optimization
   - integration with APIs or data platforms
   - measurable technical outcomes.

6. Projects MUST use strong technical terminology aligned with the Job Description keywords.
    
    Output Format (JSON):
    {{
    "contact": {{ "email": "email_address", "phone": "phone_number" }},
    "summary": "3–4 line ATS-optimized professional summary tailored to the job.",
    "education": {education_json_template},
    "experience": [
        {{
            "company": "Real Company Name 1",
            "role": "Role 1",
            "duration": "Dates",
            "description": ["Bullet 1", "Bullet 2", "Bullet 3", "Bullet 4"]
        }},
        {{
            "company": "Real Company Name 2",
            "role": "Role 2",
            "duration": "Dates",
            "description": ["..."]
        }},
        {{
            "company": "Real Company Name 3",
            "role": "Role 3",
            "duration": "Dates",
            "description": ["..."]
        }},
        {{
            "company": "Real Company Name 4",
            "role": "Role 4",
            "duration": "Dates",
            "description": ["Bullet 1", "Bullet 2", "Bullet 3", "Bullet 4"]
        }}
    ],
    "projects": [
        {{
            "title": "Project 1",
            "description": ["Line 1", "Line 2", "Line 3"]
        }},
        {{
            "title": "Project 2",
            "description": ["Line 1", "Line 2", "Line 3", "Line 4"]
        }},
        {{
            "title": "Project 3",
            "description": ["Line 1", "Line 2", "Line 3"]
        }},
        {{
            "title": "Project 4",
            "description": ["Line 1", "Line 2", "Line 3", "Line 4"]
        }}
    ],
    "skills": ["Skill 1", "Skill 2", "Skill 3", "Skill 4", "Skill 5", "Skill 6", "Skill 7", "Skill 8"],
    "certificates": ["Certification 1", "Certification 2"],
    "languages": ["Language 1", "Language 2"]
    }}
    9. TEMPLATE VARIATION & FORMATTING:
- Each generated resume must follow a VISUALLY DISTINCT formatting template style.
- You MAY use **HTML tags** for formatting within the text strings:
  • Use <b>text</b> for bold (e.g., <b>Company Name</b>).
  • Use <i>text</i> for italics (e.g., <i>Role Title</i>).
  • DO NOT use Markdown (like ** or _). Use ONLY <b> and <i> tags.
  
- Vary formatting elements such as:
  • Company names in <b>bold</b>.
  • Role titles in <i>italics</i>.
  • Section headers in ALL CAPS or Title Case.
  • Use different separator styles (|, •, —).
  
- IMPORTANT:
  • JSON structure must remain EXACTLY the same.
  • Formatting tags (<bold>, <italic>) should appear ONLY inside string values.
  • Maintain ATS compatibility.
  • The "year" in education must be a specific year (e.g., "2022") calculated as explained above.
  EXPERIENCE TIMELINE VALIDATION (MANDATORY FINAL STEP):

After generating the experience entries, you MUST perform a final validation before producing the JSON output.

1. Extract all experience durations and convert them internally into a chronological timeline.

2. Verify the following conditions:

   A. NO GAP RULE
   - There must be NO gaps between jobs.
   - If one job ends in "June 2022", the next job must start in:
        "June 2022" OR "July 2022".
   - Larger gaps are STRICTLY forbidden.

   B. NO OVERLAP RULE
   - Two jobs MUST NOT overlap.
   - Example INVALID:
        "June 2022 – March 2023"
        "January 2023 – December 2023"

   C. CHRONOLOGICAL ORDER
   - Roles must be ordered from MOST RECENT to OLDEST.
   - The last role MUST end in "Present".

   D. WORKING STUDENT RULE
   - Working Student roles MUST end exactly in:
        "December (Graduation Year - 1)"

   E. FULL TIME RULE
   - Full-time roles MUST start AFTER the Graduation Year.

3. If ANY rule is violated:
   - Automatically adjust the start/end dates to fix the timeline.
   - Ensure experience still equals the required years.

4. Only after ALL rules pass validation should the JSON output be generated.

5. The final experience timeline MUST be:
   - continuous
   - gap-free
   - non-overlapping
   - chronologically correct.
    """
    
    try:
        response = client.chat.completions.create(
            model="gpt-4o",
            messages=[{"role": "system", "content": "You are an expert resume writer."}, {"role": "user", "content": prompt}],
            response_format={"type": "json_object"}
        )
        data = json.loads(response.choices[0].message.content)
        return data
    except Exception as e:
        print(f"Error generating resume for {candidate['name']}: {e}")
        return {}

def generate_cover_letter_content(candidate: Dict, resume_data: Dict, job_description: str, education_level: str, target_country: str, client) -> str:
    company1 = resume_data.get('experience', [{}])[0].get('company', 'Unknown')
    
    # Get location from target country
    candidate_city = random.choice(COUNTRY_DATA[target_country]["cities"])
    candidate_location = f"{candidate_city}, {target_country}"
    
    current_date_obj = datetime.now()
    current_month_year = current_date_obj.strftime("%B %Y")

    prompt = f"""
  
Write a highly professional ONE-PAGE cover letter for {candidate['name']} applying for the given role.

Role: Identified from the Job Description.
Current Date: {current_month_year}.

CRITICAL OUTPUT RULES (MANDATORY):
1. START DIRECTLY with a formal salutation such as:
   "Respected Hiring Manager," or "Dear Hiring Manager,"
2. DO NOT include any header, personal details, address, email, phone number, or date.
3. DO NOT use placeholders such as "[Your Name]", "[Company Name]", or "[Date]".
4. DO NOT include meta-comments, explanations, or notes of any kind.
5. The output must read as a COMPLETE, FINAL cover letter ready for submission.

STRICT LENGTH & STRUCTURE RULES:
- The cover letter MUST contain EXACTLY **4 paragraphs**.
- Each paragraph MUST contain **exactly 6 sentences**.
- Total word count MUST be **between 550 and 600 words** so that it fits one professional page.
- Do NOT create extra paragraphs before or after the closing.
- Do NOT use bullet points or lists.
- Use polished professional prose only.

PARAGRAPH STRUCTURE (MANDATORY):

Paragraph 1 – Introduction
• Express interest in the role identified from the Job Description.
• Briefly introduce {candidate['name']}.
• Connect motivation to the company or industry.

Paragraph 2 – Education & Technical Foundation
• Mention {education_level} from {candidate.get('masters_university', 'a university')}.
• If Master's level, also mention the Bachelor of Science in Computer Science from {candidate.get('bachelors_university', 'a university')}.
• Connect academic training to the job requirements.

Paragraph 3 – Professional Experience & Impact
• Highlight achievements and responsibilities at {company1}.
• Emphasize technologies, tools, and impact relevant to the Job Description.
• Include measurable results where possible.

Paragraph 4 – Closing & Motivation
• Reinforce interest in contributing to the organization.
• Emphasize long-term motivation and professional value.
• Mention openness to further discussion and interviews.
• End with a strong professional closing sentence.

STRICT CLOSING FORMAT (MANDATORY):

After the final sentence of paragraph 4, the letter MUST end EXACTLY with:

Mit freundlichen Grüßen,
{candidate['name']}

RULES:
- The candidate name MUST appear on the line directly below "Mit freundlichen Grüßen,".
- Do NOT omit the candidate name.
- Do NOT add any text after the candidate name.
- Do NOT use other closings like "Best regards" or "Kind regards".

CONTEXT (MUST BE USED):
- Candidate Name: {candidate['name']}
- Education: {education_level}
- Professional Experience: highlight work at {company1} (MUST be distinct from the applying company)

LANGUAGE & TONE:
- Tone must be formal, persuasive, and aligned with {target_country} corporate communication standards.
- Confident but not exaggerated; factual, precise, and impact-driven.
- DO NOT mention language skills (do not mention English, German, etc.).

JOB DESCRIPTION (PRIMARY SOURCE OF TRUTH):
{job_description}

FINAL VALIDATION (MANDATORY BEFORE OUTPUT):
1. Verify that the letter contains EXACTLY 4 paragraphs.
2. Verify that each paragraph contains EXACTLY 6 sentences.
3. Verify the total word count is between 550 and 600  words.
4. Verify the closing appears EXACTLY as:

Mit freundlichen Grüßen,
{candidate['name']}

5. If ANY rule is violated, regenerate the cover letter before producing the final answer.

OUTPUT:
Return ONLY the final cover letter."""
    try:
        response = client.chat.completions.create(
            model="gpt-4o",
            messages=[{"role": "system", "content": "You are a professional career coach."}, {"role": "user", "content": prompt}]
        )
        return response.choices[0].message.content
    except Exception as e:
        print(f"Error generating cover letter for {candidate['name']}: {e}")
        return "Error generating cover letter."

def process_single_candidate(candidate: Dict, job_description: str, education_level: str, target_country: str, client) -> Tuple[Dict, Dict, str, Dict]:
    """
    Process a single candidate to generate both resume and cover letter content.
    """
    import copy
    candidate = copy.deepcopy(candidate)
    
    # Override candidate location and university based on target country
    c_data = COUNTRY_DATA[target_country]
    candidate['location'] = f"{random.choice(c_data['cities'])}, {target_country}"
    # Use the global university pool for all candidates as requested
    # Select distinct universities for Masters and Bachelors if possible
    universities = random.sample(UNIVERSITY_POOL, 2)
    candidate['masters_university'] = universities[0]
    candidate['bachelors_university'] = universities[1]
    candidate['university'] = candidate['masters_university'] # For compatibility
    
    theme = get_random_theme()
    resume_data = generate_resume_content(candidate, job_description, education_level, target_country, client)
    
    # Overwrite/Ensure contact info comes from the static candidate data
    if 'contact' not in resume_data:
        resume_data['contact'] = {}
    
    resume_data['contact']['email'] = candidate.get('email', resume_data['contact'].get('email'))
    resume_data['contact']['phone'] = candidate.get('phone', resume_data['contact'].get('phone'))
    
    cl_content = generate_cover_letter_content(candidate, resume_data, job_description, education_level, target_country, client)
    return candidate, resume_data, cl_content, theme

def create_resume_pdf(candidate: Dict, data: Dict, theme: Dict) -> bytes:
    buffer = io.BytesIO()
    # Drastically reduce margins to fit more on one page
    doc = SimpleDocTemplate(buffer, pagesize=A4, rightMargin=1.0*cm, leftMargin=1.0*cm, topMargin=1.0*cm, bottomMargin=1.0*cm)
    styles = getSampleStyleSheet()
    
    # Custom Styles
    styles.add(ParagraphStyle(name='JobTitle', parent=styles['Heading2'], spaceAfter=1, fontSize=10, leading=12))
    styles.add(ParagraphStyle(name='SectionHeader', parent=styles['Heading2'], fontSize=11, spaceAfter=2, spaceBefore=4, textColor=theme['resume_header_color']))
    styles.add(ParagraphStyle(name='NormalSmall', parent=styles['Normal'], fontSize=8.5, leading=10))
    styles.add(ParagraphStyle(name='Date', parent=styles['Normal'], fontSize=7.5, textColor=colors.gray))
    
    story = []
    
    # Header
    name_style = ParagraphStyle(
        name='NameHeader', 
        parent=styles['Heading1'], 
        fontName=theme['resume_name_font'],
        alignment=theme['resume_name_alignment']
    )
    story.append(Paragraph(candidate['name'], name_style))
    # Contact info: Email | Phone | Location
    contact_info = f"{data.get('contact', {}).get('email', '')} | {data.get('contact', {}).get('phone', '')} | {candidate['location']}"
    story.append(Paragraph(contact_info, styles['NormalSmall']))
    story.append(HRFlowable(width="100%", thickness=1, color=colors.black, spaceAfter=4))
    
    # Summary
    if 'summary' in data:
        header_text = "PROFESSIONAL SUMMARY" if theme.get('resume_header_case') == 'UPPER' else "Professional Summary"
        story.append(Paragraph(header_text, styles['SectionHeader']))
        story.append(Paragraph(data['summary'], styles['NormalSmall']))
    
    # Experience
    if 'experience' in data:
        header_text = "PROFESSIONAL EXPERIENCE" if theme.get('resume_header_case') == 'UPPER' else "Professional Experience"
        story.append(Paragraph(header_text, styles['SectionHeader']))
        for job in data['experience']:
            company_style = f"<i>{job.get('company', 'Company')}</i>" if theme['resume_company_style'] == 'Italic' else f"<b>{job.get('company', 'Company')}</b>"
            story.append(Paragraph(f"<b>{job.get('role', 'Role')}</b> at {company_style}", styles['NormalSmall']))
            story.append(Paragraph(f"{job.get('duration', '')}", styles['Date']))
            
            desc = job.get('description', [])
            if isinstance(desc, list):
                for item in desc:
                    story.append(Paragraph(f"• {clean_text_formatting(item)}", styles['NormalSmall']))
            else:
                story.append(Paragraph(clean_text_formatting(str(desc)), styles['NormalSmall']))
            story.append(Spacer(1, 2))
    
    # Projects
    if 'projects' in data:
        header_text = "KEY PROJECTS" if theme.get('resume_header_case') == 'UPPER' else "Key Projects"
        story.append(Paragraph(header_text, styles['SectionHeader']))
        for proj in data['projects']:
            # Title: Description
            # Handle user provided format which is a list of strings in description
            desc_text = ""
            if isinstance(proj.get('description'), list):
                 desc_text = "<br/>".join([f"• {clean_text_formatting(line)}" for line in proj['description']])
            else:
                 desc_text = clean_text_formatting(proj.get('description', ''))
                 
            text = f"<b>{proj.get('title', 'Project')}</b>:<br/>{desc_text}"
            story.append(Paragraph(text, styles['NormalSmall']))
            story.append(Spacer(1, 2))

    # Education
    if 'education' in data:
        header_text = "EDUCATION" if theme.get('resume_header_case') == 'UPPER' else "Education"
        story.append(Paragraph(header_text, styles['SectionHeader']))
        edu_data = data['education']
        if not isinstance(edu_data, list):
            edu_data = [edu_data]
            
        for edu in edu_data:
            story.append(Paragraph(f"<b>{edu.get('degree', 'Degree')}</b>", styles['NormalSmall']))
            story.append(Paragraph(f"{edu.get('university', 'University')} | {edu.get('year', '')}", styles['Date']))
            if 'details' in edu:
                story.append(Paragraph(edu['details'], styles['NormalSmall']))
            story.append(Spacer(1, 1))
        
    # Skills & Certificates
    if 'skills' in data or 'certificates' in data:
        header_text = "SKILLS & CERTIFICATES" if theme.get('resume_header_case') == 'UPPER' else "Skills & Certificates"
        story.append(Paragraph(header_text, styles['SectionHeader']))
        if 'skills' in data:
            skills_str = "<b>Skills:</b> " + ", ".join(data['skills'])
            story.append(Paragraph(skills_str, styles['NormalSmall']))
        if 'certificates' in data:
            cert_str = "<b>Certificates:</b> " + ", ".join(data['certificates'])
            story.append(Paragraph(cert_str, styles['NormalSmall']))
            
    # Languages
    if 'languages' in data:
        story.append(Spacer(1, 1))
        lang_str = "<b>Languages:</b> " + ", ".join(data['languages'])
        story.append(Paragraph(lang_str, styles['NormalSmall']))
    
    doc.build(story)
    buffer.seek(0)
    return buffer.getvalue()

def create_cl_pdf(candidate: Dict, content: str, theme: Dict) -> bytes:
    buffer = io.BytesIO()
    doc = SimpleDocTemplate(buffer, pagesize=A4, rightMargin=2.5*cm, leftMargin=2.5*cm, topMargin=2.5*cm, bottomMargin=2.5*cm)
    styles = getSampleStyleSheet()
    
    story = []
    # Header: Name only. 
    header_style = ParagraphStyle(
        name='CLHeader', 
        parent=styles['Heading1'], 
        alignment=theme['cl_header_alignment'], 
        textColor=theme['cl_header_color'],
        fontName=theme['cl_header_font']
    )
    story.append(Paragraph(candidate['name'], header_style))
    story.append(Spacer(1, 10))
    
    # Date: Today's date
    current_date = datetime.now().strftime("%d.%m.%Y")
    story.append(Paragraph(f"Date: {current_date}", styles['Normal']))
    story.append(HRFlowable(width="100%", thickness=1, color=colors.black, spaceAfter=20))

    for para in content.split('\n'):
        if para.strip():
            story.append(Paragraph(para.strip(), styles['Normal']))
            story.append(Spacer(1, 6))
            
    doc.build(story)
    buffer.seek(0)
    return buffer.getvalue()

def batch_generate(job_description: str, count: int, education_level: str, target_country: str, client):
    """
    Orchestrates the full generation process using concurrent execution.
    """
    min_count = 2 # Minimum to ensure diversity logic works
    if count < min_count: count = min_count
    
    candidates = generate_demographic_data(count, client)
    if not candidates:
        return None
        
    results = []
    
    # Parallelize the full profile generation (Resume + CL)
    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
        futures = [executor.submit(process_single_candidate, c, job_description, education_level, target_country, client) for c in candidates]
        
        for future in concurrent.futures.as_completed(futures):
            try:
                res = future.result() # returns (candidate, resume_data, cl_content, theme)
                results.append(res)
            except Exception as e:
                print(f"Job failed: {e}")

    # Create ZIP
    zip_buffer = io.BytesIO()
    with zipfile.ZipFile(zip_buffer, "a", compression=zipfile.ZIP_DEFLATED, allowZip64=True) as zf:
        for cand, resume_data, cl_content, theme in results:
            safe_name = "".join([c for c in cand['name'] if c.isalpha() or c.isspace()]).strip().replace(" ", "_")
            
            # Enrich candidate with contact info for CL PDF if available
            if 'contact' in resume_data:
                cand['email'] = resume_data['contact'].get('email', '')
                cand['phone'] = resume_data['contact'].get('phone', '')

            # Resume
            try:
                resume_bytes = create_resume_pdf(cand, resume_data, theme)
                zf.writestr(f"Resumes/{safe_name}_Resume.pdf", resume_bytes)
            except Exception as e:
                print(f"Error creating resume PDF for {cand['name']}: {e}")

            # Cover Letter
            try:
                cl_bytes = create_cl_pdf(cand, cl_content, theme)
                zf.writestr(f"Cover_Letters/{safe_name}_CoverLetter.pdf", cl_bytes)
            except Exception as e:
                print(f"Error creating CL PDF for {cand['name']}: {e}")
            
    zip_buffer.seek(0)
    
    # Generate timestamped filename
    timestamp = datetime.now().strftime("%m%d_%H%M%S")
    zip_filename = f"applications_{target_country.lower()}_{timestamp}.zip"
    
    return zip_buffer, results, zip_filename

def answer_screening_question(candidate: Dict, resume_data: Dict, job_description: str, question: str, client) -> str:
    """Answers an application screening question (e.g., from Indeed) using the candidate's resume and JD."""
    prompt = f"""
    You are {candidate['name']}, applying for a role based on the following Job Description:
    {job_description}

    Here is the content of your Resume (in structured JSON format for context):
    {json.dumps(resume_data)}
    
    A job portal (like Indeed) is asking you an additional screening question. 
    Answer it professionally, acting in the first person ("I").
    Base all your facts, skills, and experience exactly on the Resume data provided.
    If the question asks about a skill not on your resume, explain how your existing skills translate or state your willingness to learn, while remaining confident.
    Keep the answer concise (2-4 sentences max), as these are typical quick screening inputs.

    Question: {question}
    """
    try:
        response = client.chat.completions.create(
            model="gpt-4o",
            messages=[
                {"role": "system", "content": "You are a professional job seeker answering an application screening question."},
                {"role": "user", "content": prompt}
            ]
        )
        return response.choices[0].message.content
    except Exception as e:
        print(f"Error answering question for {candidate['name']}: {e}")
        return "Sorry, I could not generate an answer at this time."







