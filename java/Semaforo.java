package semaforo;

public class Semaforo {
	private int count;
	
	public Semaforo(int count) {
		this.count = count;
	}
	
	public void up() {
		synchronized (this) {
			count++;
			if (count == 1) {
				notify();
			}
		}
	}
	
	public void down() {
		synchronized (this) {
			while (count <= 0) {
				try {
					wait();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			count--;
		}
	}
}
