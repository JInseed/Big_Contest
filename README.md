# Big_Contest
<br>
<br>
<img src = "https://github.com/user-attachments/assets/59817506-d3f5-43f1-bca4-65511eac6085" align = "center" width = "70%">
<br>

## 진행기간
> **2022/9/3~10/17**


<br>

## 팀소개
|    소속    |          전공           |  이름  |
| :--------: | :---------------------: | :----: |
| 동국대학교 | 통계학과/데이터사이언스 | 정정룡 |
| 동국대학교 | 통계학과/데이터사이언스 | 김평진 |
| 동국대학교 | 통계학과/데이터사이언스| 황영우 |
| 동국대학교 | 통계학과| 정성훈 |
<br>

## 사용 데이터(주최 측 제공)
> **Finda App 로그정보(log_data)**

> **개인신용정보(user_spec)**

> **승인 대출상품 정보 및 대출신청여부(loan_result)**
<br>

## 개발 환경
> **R, Python**
<br>

### 역할
> **EDA, 전처리, 예측 및 군집화 모델링**
<br>

## 분석 주제
> **앱 사용성 데이터를 활용한 대출 신청 고객 예측 및 군집 맟춤형 서비스 제안**
<br>


## 분석 요약

1. user_spec(유저정보데이터) preprocessing
2. user_spec + loan_result(대출결과데이터) = merged_data
3. merged_data preprocessing(예측 모델 데이터)
4. log_data preprocessing(군집 모델 데이터)
5. credit_score 결측치 대체(Lasso, KNN-imputation, missforest)
6. 데이터 불균형 처리(SMOTE, Class weight)
7. 대출 신청 고객 예측(RF, Logistic, CatBoost)
8. 고객 군집화(DBScan)
<br>

## 분석 과정

### *데이터 형태*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/7430242f-0c36-43dc-864a-584f8cda37f4" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/a8418ef3-07c8-4020-be80-fc1f47a05875" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/aa34f7aa-721c-4099-b8a9-a5eeb13dabe5" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/f7e20a48-1699-4eeb-8833-d48129731518" width="95%">
    </td>
  </tr>
</table>
<br>

### *user_spec preprocessing*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/7e53b2d8-88b6-482e-89d1-6ec3c09f5792" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/4fbdffeb-d6e5-4d18-ba4a-cbf87376d169" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/f9719884-1d56-41d7-a260-65b175edd204" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/30023d91-bc2b-406d-a445-9a931104041d" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/0ea85ca6-4a01-4591-b322-e387c8163561" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/076efb18-2415-45f0-bb66-455f1fbb38e4" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="center">
      <img src="https://github.com/user-attachments/assets/5401c2c2-0136-486d-b6a5-447e96549776" width="48%">
    </td>
  </tr>
</table>
<br>

### *merged_data preprocessing*
<br>

<table width="100%">
  <tr>
    <td align="center">
      <img src="https://github.com/user-attachments/assets/ad019cd4-7185-43cb-b94d-916e83d818a6" width="80%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/e3b34722-7d1f-4d2b-a230-555b9b1b1224" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/c735c9d1-3956-4519-8d4f-3bbd600b0ac1" width="95%">
    </td>
  </tr>
</table>
<br>

### *log data preprocessing*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/e5d824f8-d67a-40cb-bc20-bd04f5ffe12e" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/4b74f4de-d137-4451-a771-3dc5ca95ccee" width="95%">
    </td>
  </tr>
</table>
<br>

### *credit_score 결측치 대체*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/ab145cdf-a193-42b2-a863-fb811568bc78" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/a46f6453-657f-4c8b-87a2-24071a57af40" width="95%">
    </td>
  </tr>
</table>
<br>

### *데이터 불균형 처리 및 대출 신청 고객 예측*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/9d7a58d5-dae0-4a67-beaf-0df6a04f1380" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/d31a6556-992b-465a-9260-0536d17faf1b" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/171fe515-db50-4586-912d-c2e723fd23a1" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/dd6214e6-63bb-4814-95cc-4b738f0c4d93" width="95%">
    </td>
  </tr>
</table>

<br>
<br>

### *고객군집화*
<br>

<table width="100%">
  <tr>
    <td align="left" width="50%">
      <img src="https://github.com/user-attachments/assets/39455a1a-d21a-4ba1-9884-bac2476d153b" width="95%">
    </td>
    <td align="right" width="50%">
      <img src="https://github.com/user-attachments/assets/ced43479-850c-476c-ba8c-c647d549ec95" width="95%">
    </td>
  </tr>
</table>

<table width="100%">
  <tr>
    <td align="center">
      <img src="https://github.com/user-attachments/assets/b24a98ab-08e9-4c13-88d7-b999697a8079" width="80%">
    </td>
  </tr>
</table>

<br>

- 위의 표와 같이 기대출이 존재하는 집단 및 존재하지 않은 집단을 대출 신청 횟수에 따라 모두 군집화를 진행
- 군집 간의 특징이 두드러졌으나 집단을 나누어서 군집화를 진행하여 군집이 너무 많이 발생했고 이를 해석하는 방법을 기간 내에 찾지 못해 결과물을 완성시키지 못함
<br>
<br>

## 시사점 및 보완할 점

- **전처리**
    - 고객의 금융 상품 신청 여부를 예측하는 모델이라 한 명의 고객도 빠뜨리면 안되는 제한이 있어, 결측치 처리에 많은 방법을 사용
        - 중요한 수치형 변수의 경우 MissForest, Knn imputation 등 `단일 대체법`을 활용
        - 범주형 변수의 경우 unknown이라는 `범주를 추가`시킴
            - 특정 수치형 변수는 `범주화` 시켜서 결측치 처리가 가능하도록 만듦
    - 파생변수 생성
        - 한 신청서 내의 금융 상품의 금리를 `순위화` 시켜서 파생변수 생성
            - 모델 학습 시 각 신청서 내의 금융상품 순위대로 판단할 수 있도록 설계
        - 신청서 별 상품 수를 나타내는 파생변수를 생성
            - 신청서 내에 상품 수가 많을 수록 선택지가 많은 의미로 신청할 확률에 영향을 줄 것으로 예상
        - 다양한 파생변수를 만들고 Feature Importance와 EDA를 통해 이 과정이 유의미한지 확인하는 과정이 흥미로웠음
    - 전처리를 진행 시 세세한 부분까지 고려하여 진행한 부분은 좋았으나 이 과정에서 시간 소요가 커서 결과물을 완성시키지 못함
- **분석 방법**
    - 실제 금융앱의 `로그데이터`를 다뤄보며, 세세한 데이터 정제와 통합에 관한 중요성
        - 행동별 총 누적수를 파생변수로 생성 후 군집화에 활용
        - 행동 순서에 관한 경로 분석과 같은 추가적인 방법론을 적용해보지 못해 아쉬움
    - 변수변환을 진행할 때, 범주화 시키는 경우가 많아 CatBoost 모델이 가장 성능이 좋을 것으로 예상했지만 RF 모델이 가장 좋았음. 이는 결정적으로 대출 신청 여부에 영향을 끼지는 변수는 범주형 변수가 아니었기 때문이라고 판단. 실제로 Feature Importance를 확인한 결과 연속형 변수들이 중요도가 높은 경향을 보임
- **종합**
    - 모델 예측의 성능은 입선한 팀들과 비슷했지만 군집화 부분을 완성시키지 못해 아쉬움. 너무 많은 군집이 생성되어 해석하기가 어려웠음. 이는 모든 군집을 해석하지 않고, 나누어졌던 군집을 재분류 후 묶어서 해석했다면 해결할 수 있을 것으로 판단
    - 데이터의 용량이 1GB가 넘는 대용량 데이터를 처음 만져보는 경험을 할 수 있었음. 또한 중간에 끊긴 데이터나 오류도 존재하여 주최 측에 문의하며 논리적 오류가 발생하지 않도록 다양한 처리 방식을 활용










