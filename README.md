## UTAU용 다항식 wav 생성기
utau용이라서 pcm 16bit 44100hz인데 아직 테스트 안해봄  
물론 pcm 16bit 44100hz로 출력됨 그냥 utau 에 안넣어봤을뿐  
일단 다항식은 되는데  
제대로 된 소리 만드려면 좀더 해봐야할듯   
일단 exe 올려놓을테니까 써봐요  
예상: 파일경로이슈분명히있을듯

### 그래서 뭐 어케되는거임?
여러분이 입력한 다항식 그래프를 파형으로 하는 wav가 구간반복되는것입니다  

### 입력값 설명 
file save dir : 파일 저장할 폴더 경로 입력. string  
file name : wav 파일 이름. 확장자 없이 쓰는 게 좋음. string  
interval start, interval end: x좌표 구간 시작, 끝 점. Float  
interval interval: 구간의 간격의 역수. 큰 숫자일수록 높은 음. 사실상 주파수를 결정. Int  
예를 들어 0, 8, 5 입력 시 [0, 2, 4, 6, 8] 의 x좌표가 입력됨.  
degree: 최고차항의 차수. Int  
coefficient of x^n : n차항의 계수. 아직 Int 만 됨됨  

## licence
자유롭게 사용하세요  
생성한 음원은 여러분의것입니다
